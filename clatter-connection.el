;;; clatter-connection.el --- IRC network connection management -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Network connection management for clatter.el.
;; Handles TCP/TLS connections, async I/O via process filters,
;; reconnection with exponential backoff, and health monitoring.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-socks)

(declare-function gnutls-negotiate "gnutls")

;; --- Connection Structure ---

(cl-defstruct (clatter-connection (:constructor clatter-connection--create))
  "An IRC network connection."
  network-id           ; string: network name from config
  config               ; effective plist, including `clatter-connect' overrides
  connect-overrides    ; original keyword overrides for reconnecting
  process              ; network process
  state                ; :disconnected :connecting :registering :connected
  nick                 ; current nickname
  ;; SASL
  sasl-state           ; nil :requested :authenticating :done
  cap-negotiating      ; t during CAP negotiation
  cap-enabled          ; list of enabled capability strings
  cap-available        ; list of capabilities advertised by the server
  cap-string           ; capability string sent by the server
  tagmsg-rejected      ; t once the server returned 421 for TAGMSG (stop sending)
  last-query-buffer    ; buffer that issued the most recent query command, for reply routing
  pending-whox         ; list of channels with an in-flight internal WHOX (replies are not displayed)
  ;; IRCv3 batch/label tracking
  active-batches       ; hash-table: batch-id -> plist
  deferred-batches     ; completed batches held until CAP negotiation ends
  (batch-bytes 0)       ; aggregate retained batch string bytes
  (batch-message-count 0) ; aggregate active/deferred message count
  (batch-count 0)       ; active + deferred batches, including empty batches
  pending-labels       ; hash-table: label -> callback
  label-counter        ; integer
  ;; Reconnection
  reconnect-enabled    ; t to auto-reconnect
  reconnect-attempts   ; integer
  reconnect-timer      ; timer object
  reconnect-stable-timer ; timer that resets backoff after a stable connection
  reconnect-disabled-reason ; nil :user :banned :sasl :max-attempts -- why reconnect was disabled
  disconnect-reason    ; friendly reason string for the current disconnect, shown instead of the raw sentinel event
  regain-kill-count    ; integer: consecutive "regained by services" kills
  regain-kill-time     ; float-time of last regain kill, nil if none
  desired-nick         ; string: the configured nick we want to reclaim
  nick-reclaim-timer   ; timer for periodic nick reclaim attempts
  ;; Health monitoring
  last-activity        ; float-time of last activity
  ping-sent-time       ; float-time of last health ping, nil if no pending
  health-timer         ; timer for periodic pings
  ;; Receive buffer (partial line accumulation)
  recv-buffer          ; string: incomplete data from last read
  (recv-tail-bytes 0)   ; final unfinished line, including captured/reentrant input
  ;; ISUPPORT (005) parameters
  isupport             ; hash-table: param -> value (from RPL_ISUPPORT)
  ;; AWAY (305/306) state
  away-p               ; non-nil if away
  ;; MOTD/WHOIS accumulation
  -motd-lines          ; list: accumulating MOTD lines
  -whois-data)         ; plist: accumulating WHOIS data

;; --- Active connections registry ---

(defvar clatter-connections (make-hash-table :test 'equal)
  "Hash table mapping network-id to `clatter-connection'.")

(defvar clatter-cap-complete-hook nil
  "Hook run after CAP negotiation completes.
Called with (CONN).")

(defun clatter-get-connection (network-id)
  "Get the connection for NETWORK-ID."
  (gethash network-id clatter-connections))

(defun clatter--cancel-reconnect-timer (conn)
  "Cancel CONN's pending reconnect attempt, if any."
  (when-let* ((timer (clatter-connection-reconnect-timer conn)))
    (cancel-timer timer)
    (setf (clatter-connection-reconnect-timer conn) nil)))

(defun clatter--cancel-reconnect-stable-timer (conn)
  "Cancel CONN's pending stable-connection reset, if any."
  (when-let* ((timer (clatter-connection-reconnect-stable-timer conn)))
    (cancel-timer timer)
    (setf (clatter-connection-reconnect-stable-timer conn) nil)))

(defun clatter--start-reconnect-stable-timer (conn)
  "Reset CONN's reconnect backoff after 60 seconds on the same process."
  (clatter--cancel-reconnect-stable-timer conn)
  (let ((process (clatter-connection-process conn))
        timer)
    (setq timer
          (run-at-time
           60 nil
           (lambda ()
             (when (eq timer
                       (clatter-connection-reconnect-stable-timer conn))
               (setf (clatter-connection-reconnect-stable-timer conn) nil)
               (when (and (eq process (clatter-connection-process conn))
                          (eq (clatter-connection-state conn) :connected))
                 (setf (clatter-connection-reconnect-attempts conn) 0)
                 (setf (clatter-connection-regain-kill-count conn) 0)
                 (setf (clatter-connection-regain-kill-time conn) nil))))))
    (setf (clatter-connection-reconnect-stable-timer conn) timer)))

;; --- Debug Logging ---

(defun clatter--debug (format-string &rest args)
  "Log FORMAT-STRING with ARGS if `clatter-log-raw-protocol' is non-nil."
  (when clatter-log-raw-protocol
    (with-current-buffer (get-buffer-create "*clatter-debug*")
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ")
              (apply #'format format-string args)
              "\n"))))

(defcustom clatter-watchdog-log
  (expand-file-name "clatter/watchdog.log" user-emacs-directory)
  "File path for connection watchdog log.
This log persists across Emacs restarts to help diagnose lockups."
  :type 'file
  :group 'clatter)

(defun clatter--watchdog (format-string &rest args)
  "Write a timestamped line (FORMAT-STRING with ARGS) to the watchdog log file.
Always writes regardless of `clatter-log-raw-protocol' setting."
  (let ((dir (file-name-directory clatter-watchdog-log))
        (line (concat (format-time-string "[%F %T] ")
                      (apply #'format format-string args)
                      "\n")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (write-region line nil clatter-watchdog-log 'append 'silent)))

;; --- Send ---

(defun clatter-send (conn line)
  "Send LINE to the IRC server via CONN.
Appends CR-LF automatically.  Refuses to write if the connection
appears stale (no data received for `clatter-ping-timeout' seconds)
to avoid blocking Emacs on a dead GnuTLS socket."
  (cl-block clatter-send
    (let ((proc (clatter-connection-process conn)))
      (when (and proc (process-live-p proc)
                 (memq (process-status proc) '(open run)))
        ;; Refuse to write on a likely-dead connection.  GnuTLS send can
        ;; block indefinitely at the C level if the remote peer is gone,
        ;; freezing the entire Emacs event loop.
        (let ((since-recv (- (float-time)
                             (or (clatter-connection-last-activity conn)
                                 (float-time)))))
          (when (> since-recv clatter-ping-timeout)
            (clatter--watchdog "SEND-REFUSE %s (stale %.0fs, killing)"
                               (clatter-connection-network-id conn) since-recv)
            (setf (clatter-connection-disconnect-reason conn)
                  (format "No data from server for %.0fs (connection appears dead)"
                          since-recv))
            (delete-process proc)
            (cl-return-from clatter-send nil)))
        (clatter--debug ">> %s" line)
        (run-hook-with-args 'clatter-rawlog-outgoing-hook
                            (clatter-connection-network-id conn) line)
        (let ((payload (concat line "\r\n"))
              (network-id (clatter-connection-network-id conn)))
          (condition-case err
              (with-local-quit
                (process-send-string proc payload))
            (error
             (clatter--watchdog "SEND-FAIL %s %s"
                                network-id (error-message-string err))
             (setf (clatter-connection-disconnect-reason conn)
                   (format "Connection write failed: %s"
                           (error-message-string err)))
             (delete-process proc))))))))

;; --- Receive (Process Filter) ---

(defun clatter--receive-tail-bytes (string tail-bytes)
  "Return final tail bytes after STRING, or nil if a line exceeds its limit.
TAIL-BYTES counts the unfinished line preceding this incoming chunk."
  (let* ((size (length string))
         (bytes (string-bytes string))
         (single-byte-p (= size bytes))
         (start 0))
    (while (and tail-bytes (< start size))
      (let* ((end (or (string-match "\n" string start) size))
             (chars (- end start)))
        ;; Check characters first so an oversized non-ASCII line is not copied.
        (setq tail-bytes
              (and (<= (+ tail-bytes chars) clatter-max-receive-line-bytes)
                   (+ tail-bytes
                      (cond
                       (single-byte-p chars)
                       ((and (zerop start) (= end size)) bytes)
                       (t (string-bytes (substring string start end)))))))
        (when (and tail-bytes
                   (> tail-bytes clatter-max-receive-line-bytes))
          (setq tail-bytes nil))
        (when (and tail-bytes (< end size))
          (setq tail-bytes 0))
        (setq start (min (1+ end) size))))
    tail-bytes))

(defun clatter--resource-limit-exceeded (conn resource limit)
  "Abort CONN because RESOURCE exceeded LIMIT."
  (clatter--connect-abort
   (clatter-connection-process conn)
   (format "Resource limit exceeded: %s (limit %d)" resource limit)))

(defun clatter--process-filter (proc string)
  "Process filter for incoming IRC data (STRING) from PROC.
Accumulates partial lines and dispatches complete ones."
  (let* ((network-id (process-get proc :clatter-network-id))
         (conn (clatter-get-connection network-id)))
    ;; Ignore late input from a process that no longer owns the connection.
    (when (and conn (eq proc (clatter-connection-process conn)))
      (setf (clatter-connection-last-activity conn) (float-time))
      (setq string (string-to-multibyte string))
      ;; Validate only new input.  The tail count also covers the outer filter's
      ;; captured chunk, so reentrant continuations cannot bypass the ceiling.
      (let ((tail-bytes
             (clatter--receive-tail-bytes
              string (clatter-connection-recv-tail-bytes conn))))
        (if (null tail-bytes)
            (clatter--resource-limit-exceeded
             conn "incoming line bytes" clatter-max-receive-line-bytes)
          (setf (clatter-connection-recv-tail-bytes conn) tail-bytes)
          ;; Immediately append incoming chunks to the connection buffer.
          (setf (clatter-connection-recv-buffer conn)
                (if (string-empty-p (or (clatter-connection-recv-buffer conn) ""))
                    string
                  (concat (string-to-multibyte
                           (clatter-connection-recv-buffer conn))
                          string)))
          ;; While busy, recv-buffer holds only reentrant input.  Scan the
          ;; captured chunk by offset instead of copying each remaining tail.
          (unless (process-get proc :clatter-filter-busy)
            (let ((data (clatter-connection-recv-buffer conn))
                  (start 0))
              (process-put proc :clatter-filter-busy t)
              (setf (clatter-connection-recv-buffer conn) nil)
              (unwind-protect
                  (while (and data (eq proc (clatter-connection-process conn)))
                    ;; Strip trailing CRs for both the builtin transport's LF
                    ;; and the external TLS subprocess's unconverted CRLF.
                    (while (and (eq proc (clatter-connection-process conn))
                                (string-match "\r*\n" data start))
                      (let ((line (substring data start (match-beginning 0))))
                        ;; Consume this line before any callback can reenter.
                        (setq start (match-end 0))
                        (when (> (length line) 0)
                          ;; Log raw lines to watchdog during registration.
                          (unless (eq (clatter-connection-state conn) :connected)
                            (clatter--watchdog
                             "RECV %s %s" network-id
                             (substring line 0 (min (length line) 200))))
                          (clatter--debug "<< %s" line)
                          (run-hook-with-args 'clatter-rawlog-incoming-hook
                                              network-id line)
                          (when (eq proc (clatter-connection-process conn))
                            (clatter--handle-line conn line)))))
                    (when (eq proc (clatter-connection-process conn))
                      (if (clatter-connection-recv-buffer conn)
                          ;; Older lines precede reentrant input; only their
                          ;; final partial line needs joining to that input.
                          (setq data (concat (substring data start)
                                             (clatter-connection-recv-buffer conn))
                                start 0)
                        (setf (clatter-connection-recv-buffer conn)
                              (substring data start))
                        (setq data nil))
                      (when data
                        (setf (clatter-connection-recv-buffer conn) nil))))
                ;; Preserve undispatched bytes on error, but never restore data
                ;; after a callback disconnects or replaces the process.
                (when (and data (eq proc (clatter-connection-process conn)))
                  (setf (clatter-connection-recv-buffer conn)
                        (concat (substring data start)
                                (or (clatter-connection-recv-buffer conn) ""))))
                (process-put proc :clatter-filter-busy nil)))))))))

(defun clatter--handle-line (conn line)
  "Parse and handle a single IRC LINE on CONN.
This is the main entry point from the process filter into the handler layer."
  (let ((msg (clatter-parse-line line)))
    (when msg
      ;; Dispatch to handlers (defined in clatter-handlers.el)
      (clatter-dispatch-message conn msg))))

;; Forward declaration - implemented in clatter-handlers.el
(declare-function clatter-dispatch-message "clatter-handlers")

;; --- Process Sentinel (disconnect handling) ---

(defun clatter--classify-disconnect-event (event)
  "Translate a raw process-sentinel EVENT string into a human-readable cause.
Emacs sentinel events are terse (\"connection broken by remote peer\",
\"finished\", \"deleted process\"); this maps the known ones to clearer text
and returns the trimmed event unchanged for anything unrecognized, so no
information is lost."
  (let ((e (string-trim event)))
    (cond
     ((string-match-p "broken by remote peer" e) "Connection closed by remote peer")
     ((string-match-p "broken by peer" e) "Connection closed by peer")
     ((string-equal e "finished") "Connection process exited")
     ((string-equal e "deleted process") "Connection closed")
     (t e))))

(defun clatter--disconnect-message (conn event)
  "Return a friendly disconnect message for CONN and sentinel EVENT.
Prefers a reason set by the caller via the `disconnect-reason' slot (for
internal disconnects such as health kills, connect aborts, and explicit
disconnects) and clears that slot; otherwise classifies the raw EVENT."
  (or (prog1 (clatter-connection-disconnect-reason conn)
        (setf (clatter-connection-disconnect-reason conn) nil))
      (clatter--classify-disconnect-event event)))

(defun clatter--clear-session-data (conn)
  "Release receive and reply accumulation owned by CONN's dead session.
Keep network configuration, buffers, and displayed history intact."
  (setf (clatter-connection-recv-buffer conn) nil
        (clatter-connection-recv-tail-bytes conn) 0
        (clatter-connection-deferred-batches conn) nil
        (clatter-connection-batch-bytes conn) 0
        (clatter-connection-batch-message-count conn) 0
        (clatter-connection-batch-count conn) 0
        (clatter-connection--motd-lines conn) nil
        (clatter-connection--whois-data conn) nil
        (clatter-connection-pending-whox conn) nil
        (clatter-connection-last-query-buffer conn) nil)
  (when (clatter-connection-active-batches conn)
    (clrhash (clatter-connection-active-batches conn)))
  (when (clatter-connection-pending-labels conn)
    (clrhash (clatter-connection-pending-labels conn))))

(defun clatter--process-sentinel (proc event)
  "Process sentinel for PROC handling disconnect EVENT."
  (let* ((network-id (process-get proc :clatter-network-id))
         (conn (clatter-get-connection network-id)))
    ;; A superseded process can report its terminal event after a replacement
    ;; is already installed.  It no longer owns this connection's state.
    (when (and conn (eq proc (clatter-connection-process conn)))
      (clatter--debug "sentinel: %s %s" network-id (string-trim event))
      (clatter--watchdog "DISCONNECT %s event=%s" network-id (string-trim event))
      ;; Compute the user-facing reason once: callers may have set a friendly
      ;; `disconnect-reason' before deleting the process (health kills, connect
      ;; aborts, explicit disconnects); otherwise classify the raw sentinel
      ;; event.  `clatter--disconnect-message' clears the slot as a side effect
      ;; so a later disconnect starts fresh; the local REASON binding carries
      ;; the value to the UI hook below without re-polluting the slot.
      (let ((reason (clatter--disconnect-message conn event)))
        ;; The first disconnect of a cycle deserves a notice; during a
        ;; backoff cycle the mode-line "(reconnect Ns)" indicator is the
        ;; unobtrusive status, so keep the echo area quiet.
        (when (= (clatter-connection-reconnect-attempts conn) 0)
          (message "[clatter] Disconnected from %s: %s" network-id reason))
        (setf (clatter-connection-state conn) :disconnected)
        (setf (clatter-connection-process conn) nil)
        (clatter--clear-session-data conn)
        ;; Release the dead process so it does not linger in
        ;; `process-list' (M-x list-processes) after every disconnect.
        ;; This sentinel only runs on terminal events, so PROC is already
        ;; dead; `delete-process' is idempotent when other paths (the
        ;; connect-abort/watchdog/health paths) already deleted it.
        (delete-process proc)
        ;; The external-TLS subprocess captures gnutls-cli/openssl stderr in
        ;; a hidden buffer; kill it now that the subprocess is gone so failed
        ;; connections don't leave one buffer per attempt.  Failure reasons are
        ;; already preserved in the watchdog log.
        (when-let* ((tls-buf (get-buffer (format " *clatter-tls-%s*" network-id))))
          (kill-buffer tls-buf))
        (clatter--cancel-reconnect-stable-timer conn)
        ;; Cancel health timer
        (when (clatter-connection-health-timer conn)
          (cancel-timer (clatter-connection-health-timer conn))
          (setf (clatter-connection-health-timer conn) nil))
        ;; Cancel nick reclaim timer
        (when (clatter-connection-nick-reclaim-timer conn)
          (cancel-timer (clatter-connection-nick-reclaim-timer conn))
          (setf (clatter-connection-nick-reclaim-timer conn) nil))
        ;; Notify UI with the friendly reason.
        (run-hook-with-args 'clatter-disconnect-hook network-id reason)
        ;; Auto-reconnect
        (when (clatter-connection-reconnect-enabled conn)
          (let ((attempts (clatter-connection-reconnect-attempts conn)))
            (clatter--watchdog "RECONNECT-SCHEDULE %s attempt=%d delay=%ds"
                               network-id (1+ attempts)
                               (min (* clatter-reconnect-initial-delay
                                       (expt 2 attempts))
                                    clatter-reconnect-max-delay))
            (clatter--schedule-reconnect conn)))))))

;; --- External TLS helper subprocess ---

(defun clatter--tls-external-args (server port client-cert)
  "Build the command list for an external TLS client to SERVER:PORT.
CLIENT-CERT, when non-nil and existing, is presented to the server.
The program is selected by `clatter-tls-external-command'."
  (let ((have-cert (and client-cert (file-exists-p client-cert))))
    (pcase clatter-tls-external-command
      ("gnutls-cli"
       (append (list "gnutls-cli" "--port" (number-to-string port))
               (when have-cert
                 (list (concat "--x509certfile=" client-cert)
                       (concat "--x509keyfile=" client-cert)))
               (list server)))
      (_
       ;; s_client does not abort on certificate verification failure
       ;; without -verify_return_error, and checks no hostname without
       ;; -verify_hostname; both are required to resist MITM.
       (append (list (if (equal clatter-tls-external-command "openssl")
                         "openssl" clatter-tls-external-command)
                     "s_client" "-quiet"
                     "-connect" (format "%s:%d" server port)
                     "-servername" server
                     "-verify_return_error"
                     "-verify_hostname" server)
               (when have-cert
                 (list "-cert" client-cert "-key" client-cert)))))))

(defun clatter--make-external-tls-process (network-id server port client-cert)
  "Spawn an external TLS subprocess for NETWORK-ID connecting to SERVER:PORT.
CLIENT-CERT is an optional client certificate file.  Returns the process.
IRC is spoken in plaintext over the subprocess pipe, so a dead network
socket blocks the subprocess instead of Emacs's event loop."
  (let* ((cmd (clatter--tls-external-args server port client-cert))
         (program (car cmd)))
    (unless (executable-find program)
      (error "External TLS client not found: %s" program))
    ;; Force line-buffered output so incoming IRC lines are not delayed by
    ;; the helper's stdio block buffering when its stdout is a pipe.
    (when (executable-find "stdbuf")
      (setq cmd (append (list "stdbuf" "-oL" "-eL") cmd)))
    (make-process
     :name (format "clatter-%s" network-id)
     :command cmd
     :coding '(utf-8 . utf-8)
     :connection-type 'pipe
     :noquery t
     :filter #'clatter--process-filter
     :sentinel #'clatter--process-sentinel
     :stderr (get-buffer-create (format " *clatter-tls-%s*" network-id)))))

(defun clatter--connect-external (conn config network-id server port)
  "Connect CONN to SERVER:PORT via an external TLS subprocess.
CONFIG is the network plist; NETWORK-ID names the connection.
Begins IRC registration once the subprocess is spawned."
  (let ((proc (clatter--make-external-tls-process
               network-id server port (plist-get config :client-cert))))
    (setf (clatter-connection-process conn) proc)
    (process-put proc :clatter-network-id network-id)
    (clatter--watchdog "EXT-TLS-SPAWN %s via %s"
                       network-id clatter-tls-external-command)
    ;; The subprocess performs the TLS handshake; any plaintext we write
    ;; is queued in the pipe and flushed once the tunnel is up, so it is
    ;; safe to begin IRC registration immediately.
    (clatter--begin-registration conn config)
    ;; Readiness watchdog: if not fully connected (001 received) within
    ;; 30s, tear down the subprocess and let reconnect logic take over.
    (let ((wp proc))
      (run-at-time 30 nil
                   (lambda ()
                     (when (and (process-live-p wp)
                                (not (eq (clatter-connection-state conn) :connected)))
                       (clatter--watchdog "EXT-WATCHDOG-KILL %s (not connected in 30s)"
                                          network-id)
                       (setf (clatter-connection-disconnect-reason conn)
                             "Connection attempt timed out (30s)")
                       (delete-process wp)))))
    conn))

(defun clatter--connect-abort (proc reason)
  "Report REASON and delete in-progress PROC.
Deleting PROC triggers its sentinel, which sets the disconnected state and
schedules any reconnect, so callers must not duplicate that work here.
REASON is also stashed on the connection as a friendly `disconnect-reason'
so the sentinel surfaces it instead of the raw sentinel event."
  (message "[clatter] %s" reason)
  (let* ((network-id (process-get proc :clatter-network-id))
         (conn (and network-id (clatter-get-connection network-id))))
    (when conn
      (setf (clatter-connection-disconnect-reason conn) reason)))
  (when (process-live-p proc)
    (delete-process proc)))

(defun clatter--after-tcp-open (conn proc config server use-tls)
  "Continue connecting CONN once the TCP/tunnel to SERVER is open on PROC.
Negotiate TLS when USE-TLS, then begin IRC registration.  CONFIG is the merged
network configuration."
  (let ((network-id (clatter-connection-network-id conn))
        (ok t))
    (when use-tls
      (clatter--watchdog "TLS-START %s" network-id)
      (condition-case tls-err
          (with-timeout (10
                         (clatter--watchdog "TLS-TIMEOUT %s" network-id)
                         (error "TLS handshake timed out (10s)"))
            (gnutls-negotiate
             :process proc
             :hostname server
             ;; NSM at its default 'medium level checks CA trust but not
             ;; hostname mismatches; require strict hostname verification
             ;; so a valid cert for the wrong host cannot MITM the session.
             :verify-hostname-strict t
             :keylist (let ((client-cert (plist-get config :client-cert)))
                        (when (and client-cert (file-exists-p client-cert))
                          (list (list client-cert client-cert))))))
        (error
         (setq ok nil)
         (clatter--watchdog "TLS-FAIL %s %s" network-id
                            (error-message-string tls-err))
         (clatter--connect-abort
          proc (format "TLS failed: %s" (error-message-string tls-err))))))
    (when (and ok (process-live-p proc))
      (clatter--watchdog "TLS-OK %s" network-id)
      (set-process-sentinel proc #'clatter--process-sentinel)
      (clatter--begin-registration conn config))))

;; --- Connect ---
(defvar clatter--in-auto-retry nil
  "Dynamically bound to t while `clatter-connect' runs from a reconnect timer.
An auto-retry continues the current backoff cycle instead of starting a
fresh one, so `clatter-connect' leaves `reconnect-attempts' alone.")

(defun clatter-connect (network-id &rest args)
  "Connect to IRC network NETWORK-ID.
ARGS are keyword arguments that override `clatter-networks' config:
  :server :port :tls :nick :username :realname :sasl :client-cert
  :autojoin :password :bouncer"
  (interactive
   (list (completing-read "Network: "
                          (mapcar #'car clatter-networks))))
  ;; Merge args with stored config
  (let* ((stored (cdr (assoc network-id clatter-networks #'equal)))
         (config (if args (append args stored) stored))
         (server (plist-get config :server))
         (port (or (plist-get config :port) clatter-default-port))
         (use-tls (if (plist-member config :tls)
                      (plist-get config :tls)
                    clatter-default-tls))
         (nick (or (plist-get config :nick) clatter-default-nick))
         (proxy (clatter-proxy-config config)))
    (unless server
      (error "No server specified for network %s" network-id))
    (unless nick
      (error "No nick specified for network %s" network-id))

    ;; STS policy enforcement: force TLS if cached policy exists
    (when (and (fboundp 'clatter-sts-lookup)
               (not use-tls))
      (let ((sts-policy (clatter-sts-lookup server)))
        (when sts-policy
          (setq use-tls t)
          (setq port (plist-get sts-policy :port))
          (clatter--debug "STS: enforcing TLS on port %d for %s" port server))))

    ;; Record the resolved TLS flag (explicit config, default, or an STS
    ;; upgrade above) on the effective config.  CAP negotiation gates
    ;; password-bearing SASL on it, and STS logic keys its upgrade check
    ;; on it, so it must reflect reality even when :tls was defaulted.
    (setq config (append (list :tls use-tls) config))

    (when (and proxy (not (and (plist-get proxy :host) (plist-get proxy :port))))
      (error "Invalid SOCKS proxy: both :host and :port are required"))

    (when (and proxy use-tls (eq clatter-tls-method 'external))
      (error "SOCKS proxy requires builtin TLS (set `clatter-tls-method' to \\='builtin)"))

    ;; Fail fast when SASL credentials are configured but unavailable.
    ;; A nil password would otherwise make clatter silently register
    ;; anonymous (skipping SASL); a SASL-required server then drops the
    ;; connection, and the reconnect loop retries a failure only the user
    ;; can fix.  Refuse up front and point at the cause instead.
    (if (and (memq (plist-get config :sasl) '(plain scram-sha-256))
             (not (clatter-get-password network-id config)))
        (progn
          (clatter--watchdog "CONNECT-REFUSE %s no SASL password" network-id)
          (message "[clatter] No password found for %s (:sasl %s); not connecting.  Fix auth-source/pass and run M-x clatter-connect."
                   network-id (plist-get config :sasl))
          (let ((conn (clatter-get-connection network-id)))
            (when conn
              (setf (clatter-connection-reconnect-enabled conn) nil)
              (setf (clatter-connection-reconnect-disabled-reason conn) :sasl)
              (clatter--cancel-reconnect-timer conn)
              (setf (clatter-connection-state conn) :disconnected)
              (run-hook-with-args 'clatter-system-hook conn
                                 (format "No SASL password found for %s"
                                         network-id))))
          nil)
      ;; Create or reuse connection struct
      (let ((conn (or (clatter-get-connection network-id)
                      (let ((new-conn (clatter-connection--create
                                       :network-id network-id
                                       :config config
                                       :connect-overrides args
                                       :state :disconnected
                                       :nick nick
                                       :reconnect-enabled t
                                       :reconnect-attempts 0
                                       :active-batches (make-hash-table :test 'equal)
                                       :pending-labels (make-hash-table :test 'equal)
                                       :label-counter 0)))
                        (puthash network-id new-conn clatter-connections)
                        new-conn))))

        ;; Detach a superseded process before deleting it so its sentinel cannot
        ;; schedule a second reconnect for the new attempt.
        (clatter--cancel-reconnect-timer conn)
        (clatter--cancel-reconnect-stable-timer conn)
        (when-let* ((old-process (clatter-connection-process conn)))
          (setf (clatter-connection-process conn) nil)
          (delete-process old-process))
        (clatter--clear-session-data conn)

        (setf (clatter-connection-state conn) :connecting)
        ;; Keep the resolved config after the process is gone so reconnects
        ;; retain temporary `clatter-connect' keyword overrides (notably
        ;; `:bouncer').
        (setf (clatter-connection-config conn) config)
        (setf (clatter-connection-connect-overrides conn) args)
        (setf (clatter-connection-nick conn) nick)
        (setf (clatter-connection-desired-nick conn) nick)
        ;; An explicit connect (manual or fan-out) starts a fresh cycle:
        ;; re-enable auto-reconnect, clear any give-up state, and reset the
        ;; backoff counters.  Without the attempt reset, connecting after a
        ;; give-up would hit the cap again on its first failure and give up
        ;; immediately -- the reported "no reconnect after the 10th try" bug.
        ;; A timer-driven auto-retry keeps the current count so the backoff
        ;; cycle and give-up cap keep advancing.
        (unless clatter--in-auto-retry
          (setf (clatter-connection-reconnect-enabled conn) t)
          (setf (clatter-connection-reconnect-disabled-reason conn) nil)
          (setf (clatter-connection-reconnect-attempts conn) 0)
          (setf (clatter-connection-regain-kill-count conn) 0)
          (setf (clatter-connection-regain-kill-time conn) nil))
        (setf (clatter-connection-cap-negotiating conn) nil)
        (setf (clatter-connection-cap-enabled conn) nil)
        (setf (clatter-connection-cap-string conn) nil)
        (setf (clatter-connection-tagmsg-rejected conn) nil)
        (setf (clatter-connection-sasl-state conn) nil)
        (setf (clatter-connection-ping-sent-time conn) nil)
        (setf (clatter-connection-away-p conn) nil)

        (unless clatter--in-auto-retry
          (message "[clatter] Connecting to %s:%d%s..."
                   server port (if use-tls " (TLS)" "")))
        (clatter--watchdog "CONNECT %s %s:%d tls=%s" network-id server port use-tls)

        (condition-case err
            (if (and use-tls (eq clatter-tls-method 'external))
                ;; External TLS subprocess path: TLS runs in a separate OS
                ;; process so a dead socket cannot block Emacs's event loop.
                (clatter--connect-external conn config network-id server port)
            (let* (;; Always connect plain+nowait first - TLS handshake
                   ;; happens async in the sentinel to avoid blocking Emacs
                   (proc (make-network-process
                          :name (format "clatter-%s" network-id)
                          :host (if proxy (plist-get proxy :host) server)
                          :service (if proxy (plist-get proxy :port) port)
                          :nowait t
                          :coding (if proxy '(binary . binary) '(utf-8 . utf-8))
                          :filter #'clatter--process-filter
                          :sentinel
                          (lambda (p e)
                            (clatter--watchdog "SENTINEL %s event=%s status=%s"
                                               network-id (string-trim e)
                                               (process-status p))
                            (cond
                             ;; TCP connected - run SOCKS handshake or go direct
                             ((string-match-p "open" e)
                              (if proxy
                                  (clatter-socks-begin
                                   p server port proxy
                                   (lambda ()
                                     (set-process-coding-system p 'utf-8 'utf-8)
                                     (set-process-filter p #'clatter--process-filter)
                                     (clatter--watchdog "SOCKS-OK %s" network-id)
                                     (clatter--after-tcp-open conn p config server use-tls))
                                   (lambda (reason)
                                     (clatter--watchdog "SOCKS-FAIL %s %s" network-id reason)
                                     (clatter--connect-abort
                                      p (format "SOCKS5: %s" reason))))
                                (clatter--after-tcp-open conn p config server use-tls)))
                             ;; Connection failed or closed
                             (t
                              (clatter--process-sentinel p e)))))))
              (setf (clatter-connection-process conn) proc)
              (process-put proc :clatter-network-id network-id)
              ;; Watchdog: kill process if still connecting after 15 seconds
              (let ((watchdog-proc proc))
                (run-at-time 15 nil
                             (lambda ()
                               (when (and (process-live-p watchdog-proc)
                                          (eq (clatter-connection-state conn) :connecting))
                                 (clatter--watchdog "WATCHDOG-KILL %s (stuck connecting)" network-id)
                                 (setf (clatter-connection-disconnect-reason conn)
                                       "Connection attempt timed out (15s)")
                                 (delete-process watchdog-proc)))))
              conn))
          (error
           (clatter--watchdog "CONNECT-FAIL %s %s" network-id (error-message-string err))
           (setf (clatter-connection-state conn) :disconnected)
           (unless clatter--in-auto-retry
             (message "[clatter] Connection failed: %s" (error-message-string err)))
           ;; A synchronous `make-network-process' failure (bad host,
           ;; immediate DNS failure) never reaches the sentinel, so
           ;; schedule the retry here or the connection stays dead.
           (when (clatter-connection-reconnect-enabled conn)
             (clatter--schedule-reconnect conn))
           nil))))))

;; --- Registration ---

(defun clatter--begin-registration (conn config)
  "Begin IRC registration sequence on CONN with CONFIG.
Always starts with CAP LS 302 for IRCv3 negotiation."
  (clatter--watchdog "REGISTER-START %s" (clatter-connection-network-id conn))
  (setf (clatter-connection-state conn) :registering)
  (setf (clatter-connection-cap-negotiating conn) t)
  (setf (clatter-connection-last-activity conn) (float-time))
  ;; Start health monitor
  (clatter--start-health-timer conn)
  ;; Store config on connection for CAP handler to reference
  (process-put (clatter-connection-process conn) :clatter-config config)
  ;; Always start with CAP negotiation
  (clatter-send conn "CAP LS 302"))

;; --- Disconnect ---

(defun clatter-disconnect (network-id &optional message)
  "Disconnect from NETWORK-ID with optional quit MESSAGE."
  (interactive
   (list (completing-read "Disconnect: "
                          (cl-loop for k being the hash-keys of clatter-connections
                                   collect k))))
  (let ((conn (clatter-get-connection network-id)))
    (when conn
      (setf (clatter-connection-reconnect-enabled conn) nil)
      (setf (clatter-connection-reconnect-disabled-reason conn) :user)
      (clatter--cancel-reconnect-timer conn)
      (clatter--cancel-reconnect-stable-timer conn)
      (when (clatter-connection-nick-reclaim-timer conn)
        (cancel-timer (clatter-connection-nick-reclaim-timer conn))
        (setf (clatter-connection-nick-reclaim-timer conn) nil))
      (let ((proc (clatter-connection-process conn)))
        (when (and proc (process-live-p proc))
          (clatter-send conn (clatter-irc-quit (or message clatter-quit-message)))
          ;; Give the QUIT time to send, then delete async.  The sentinel will
          ;; surface a friendly disconnect reason to the UI.
          (setf (clatter-connection-disconnect-reason conn)
                (if message
                    (format "Disconnected (%s)" message)
                  "Disconnected by user"))
          (run-at-time 0.5 nil
                       (lambda ()
                         (when (process-live-p proc)
                           (delete-process proc))))))
      (setf (clatter-connection-state conn) :disconnected)
      (message "[clatter] Disconnected from %s" network-id))))

(defun clatter-disconnect-all (&optional message)
  "Disconnect from all connected networks with optional quit MESSAGE.
Intended for use on Emacs exit: sends QUIT synchronously and deletes
each process so no orphaned ghost sessions remain on the servers."
  (let ((quit-msg (or message clatter-quit-message)))
    (maphash
     (lambda (_network-id conn)
       (setf (clatter-connection-reconnect-enabled conn) nil)
       (setf (clatter-connection-reconnect-disabled-reason conn) :user)
       (clatter--cancel-reconnect-timer conn)
       (clatter--cancel-reconnect-stable-timer conn)
       (when (clatter-connection-nick-reclaim-timer conn)
         (cancel-timer (clatter-connection-nick-reclaim-timer conn))
         (setf (clatter-connection-nick-reclaim-timer conn) nil))
       (let ((proc (clatter-connection-process conn)))
         (when (and proc (process-live-p proc)
                    (memq (process-status proc) '(open run)))
           (clatter--watchdog "QUIT-ON-EXIT %s"
                              (clatter-connection-network-id conn))
           ;; Send QUIT synchronously.  We cannot rely on async timers
           ;; here because Emacs is exiting.
           (ignore-errors
             (with-local-quit
               (process-send-string proc
                                    (concat (clatter-irc-quit quit-msg) "\r\n"))
               ;; Briefly let the QUIT flush to the socket.
               (accept-process-output proc 0.3)))
           (ignore-errors (delete-process proc)))
         (setf (clatter-connection-process conn) nil)
         (setf (clatter-connection-state conn) :disconnected)))
     clatter-connections)))

(defun clatter--quit-on-exit ()
  "Cleanly disconnect all networks when Emacs exits.
Added to `kill-emacs-hook'.  No-op unless `clatter-quit-on-exit'."
  (when clatter-quit-on-exit
    (clatter-disconnect-all)))

;; `clatter--quit-on-exit' is installed on `kill-emacs-hook' by
;; `clatter-setup', so that merely loading clatter adds no global hooks.

;; --- Reconnection ---

(defun clatter--schedule-reconnect (conn)
  "Schedule a reconnection attempt for CONN with exponential backoff.
If the last disconnect was a recent services nick-regain kill, the
delay is raised to at least `clatter-regain-kill-backoff' seconds to
avoid an immediate re-collision and another kill."
  (unless (or (eq (clatter-connection-state conn) :connecting)
              (clatter-connection-reconnect-timer conn))
    (let* ((network-id (clatter-connection-network-id conn))
           (attempts (clatter-connection-reconnect-attempts conn)))
      (if (and clatter-reconnect-max-attempts
               (>= attempts clatter-reconnect-max-attempts))
          ;; A persistent, user-actionable failure (unavailable credentials,
          ;; SASL-required server) will never succeed by retrying.  Stop the
          ;; loop and tell the user how to recover.
          (progn
            (setf (clatter-connection-reconnect-enabled conn) nil)
            (setf (clatter-connection-reconnect-disabled-reason conn) :max-attempts)
            (clatter--watchdog "RECONNECT-GIVEUP %s attempts=%d" network-id attempts)
            (message "[clatter] Giving up reconnecting to %s after %d attempts; run M-x clatter-connect once the problem is fixed"
                     network-id attempts)
            (run-hook-with-args 'clatter-system-hook conn
                               (format "Reconnect attempts exhausted for %s"
                                       network-id)))
        (clatter--schedule-reconnect--schedule conn attempts)))))

(defun clatter--schedule-reconnect--schedule (conn attempts)
  "Schedule a reconnection attempt for CONN.
ATTEMPTS is the pre-increment reconnect attempt count.  Applies exponential
backoff with the regain-kill backoff override; see `clatter--schedule-reconnect'
for the give-up cap."
  (let* ((delay (min (* clatter-reconnect-initial-delay (expt 2 attempts))
                     clatter-reconnect-max-delay))
         (kill-time (clatter-connection-regain-kill-time conn))
         (recent-regain-kill (and kill-time
                                  (< (- (float-time) kill-time) 30)))
         (delay (if recent-regain-kill
                    (max delay clatter-regain-kill-backoff)
                  delay))
         (network-id (clatter-connection-network-id conn))
         (overrides (clatter-connection-connect-overrides conn)))
    (when recent-regain-kill
      (clatter--watchdog "RECONNECT-BACKOFF %s regain-kill-count=%d delay=%ds"
                         network-id
                         (or (clatter-connection-regain-kill-count conn) 0)
                         delay))
    ;; One echo-area notice per cycle (the first attempt); the mode-line
    ;; "(reconnect Ns)" fragment tracks later attempts unobtrusively.
    (when (= attempts 0)
      (message "[clatter] Reconnecting to %s in %ds..."
               network-id delay))
    (run-hook-with-args 'clatter-reconnect-hook network-id delay (1+ attempts))
    (setf (clatter-connection-reconnect-attempts conn) (1+ attempts))
    (let (timer)
      (setq timer
            (run-at-time
             delay nil
             (lambda ()
               (when (eq timer (clatter-connection-reconnect-timer conn))
                 (setf (clatter-connection-reconnect-timer conn) nil)
                 (let ((clatter--in-auto-retry t))
                   (apply #'clatter-connect network-id overrides))))))
      (setf (clatter-connection-reconnect-timer conn) timer))))

;; --- Health Monitoring ---

(defun clatter--start-health-timer (conn)
  "Start periodic health check timer for CONN."
  (when (clatter-connection-health-timer conn)
    (cancel-timer (clatter-connection-health-timer conn)))
  (setf (clatter-connection-health-timer conn)
        (run-at-time clatter-ping-interval clatter-ping-interval
                     #'clatter--health-check conn)))

(defun clatter--health-check (conn)
  "Check connection health for CONN.
Like ERC, uses a single timeout on last-received-time:
- If no data received for `clatter-ping-timeout' seconds, kill the
  connection (it is dead).
- Otherwise, send a PING to keep the connection alive and provoke
  a PONG that resets the receive timer.
PINGs are only sent when state is :connected (after 001)."
  (let* ((proc (clatter-connection-process conn))
         (network-id (clatter-connection-network-id conn)))
    (cond
     ;; Process gone -- cancel our own timer
     ((not (and proc (process-live-p proc)
                 (memq (process-status proc) '(open run))))
      (clatter--watchdog "HEALTH-STALE %s (process gone)" network-id)
      (when (clatter-connection-health-timer conn)
        (cancel-timer (clatter-connection-health-timer conn))
        (setf (clatter-connection-health-timer conn) nil)))
     ;; Not yet registered -- don't send PINGs, let watchdog handle stuck state
     ((not (eq (clatter-connection-state conn) :connected))
      nil)
     ;; Connected -- check health
     (t
      (let ((since-recv (- (float-time)
                           (or (clatter-connection-last-activity conn) 0))))
        (cond
         ;; No data received for too long -- connection is dead
         ((> since-recv clatter-ping-timeout)
          (clatter--watchdog "HEALTH-KILL %s (no data for %.0fs, timeout %ds)"
                             network-id since-recv clatter-ping-timeout)
          (message "[clatter] No data from %s for %.0fs, killing connection"
                   network-id since-recv)
          (setf (clatter-connection-ping-sent-time conn) nil)
          (setf (clatter-connection-disconnect-reason conn)
                (format "No data from server for %.0fs (connection appears dead)"
                        since-recv))
          (delete-process proc))
         ;; Send keepalive PING
         (t
          (clatter--watchdog "HEALTH-PING %s (last-recv %.0fs ago)"
                             network-id since-recv)
          (setf (clatter-connection-ping-sent-time conn) (float-time))
          (clatter-send conn (clatter-irc-ping "clatter")))))))))

;; --- Suspend / Resume detection ---

;; A single periodic heartbeat infers a system suspend from a large gap in
;; wall-clock time between fires.  Emacs timers do not advance while the
;; machine is suspended, so on resume the next fire lands far behind schedule;
;; that gap is the signal that the network is almost certainly half-open and
;; the normal 120s health-check timeout would be far too slow.

(defvar clatter--resume-timer nil
  "Periodic heartbeat timer used to detect system suspend/resume.
Nil when detection is off.")

(defvar clatter--last-heartbeat 0
  "Float-time of the last heartbeat fire, or 0 before the first one.")

(defun clatter--resume-heartbeat ()
  "Heartbeat callback: infer a suspend from a gap in wall-clock time.
Compares the time since the last fire to `clatter-suspend-threshold' and,
on a detected resume, triggers prompt reconnection for every connection."
  (let ((now (float-time)))
    ;; Skip the gap check on the very first fire (last-heartbeat is 0) so a
    ;; freshly started detector does not mistake startup for a resume.
    (when (and (> clatter--last-heartbeat 0)
               (> (- now clatter--last-heartbeat) clatter-suspend-threshold))
      (clatter--watchdog "RESUME gap=%.0fs threshold=%ds"
                         (- now clatter--last-heartbeat) clatter-suspend-threshold)
      (clatter--on-resume))
    (setq clatter--last-heartbeat now)))

(defun clatter--reschedule-after-resume (conn)
  "Re-establish CONN promptly after a detected resume.
A suspend is not a server-side persistent failure, so the reconnect
backoff is reset.  Half-open (still \"live\") processes are killed so the
sentinel reschedules with the reset backoff.  A pending backoff timer is
replaced by a fresh schedule.  A connection that gave up purely because
it hit `clatter-reconnect-max-attempts' (a transient failure, often
suspend-induced) is given one more chance; explicit disconnects, bans,
and SASL refusals are left down."
  (let ((network-id (clatter-connection-network-id conn)))
    ;; A suspend is transient: do not let it compound the backoff or the
    ;; regain-kill bookkeeping from before the machine slept.
    (setf (clatter-connection-reconnect-attempts conn) 0)
    (setf (clatter-connection-regain-kill-count conn) 0)
    (setf (clatter-connection-regain-kill-time conn) nil)
    (let ((proc (clatter-connection-process conn)))
      (cond
       ;; A live process is almost certainly half-open after a suspend:
       ;; tear it down; the sentinel reschedules with the reset backoff.
       ((and proc (process-live-p proc)
             (memq (process-status proc) '(open run)))
        (setf (clatter-connection-disconnect-reason conn)
              "Resuming after system suspend")
        (clatter--watchdog "RESUME-KILL %s (half-open after suspend)" network-id)
        (delete-process proc))
       ;; Disconnected with a pending backoff timer: replace it with a
       ;; prompt retry using the reset backoff.
       ((clatter-connection-reconnect-timer conn)
        (clatter--cancel-reconnect-timer conn)
        (when (clatter-connection-reconnect-enabled conn)
          (clatter--schedule-reconnect conn)))
       ;; Disconnected, no pending timer, but auto-reconnect was given up
       ;; purely as a transient max-attempts failure: revive it once.
       ((and (not (clatter-connection-reconnect-enabled conn))
             (eq (clatter-connection-reconnect-disabled-reason conn)
                 :max-attempts))
        (setf (clatter-connection-reconnect-enabled conn) t)
        (setf (clatter-connection-reconnect-disabled-reason conn) nil)
        (clatter--watchdog "RESUME-REVIVE %s (transient give-up)" network-id)
        (clatter--schedule-reconnect conn))))))

(defun clatter--on-resume ()
  "Re-establish every connection after a detected system resume."
  (maphash (lambda (_network-id conn)
             (clatter--reschedule-after-resume conn))
           clatter-connections))

(defun clatter--start-resume-detector ()
  "Start the suspend/resume heartbeat detector if not already running.
The detector fires every `clatter-ping-interval' seconds."
  (unless clatter--resume-timer
    (setq clatter--last-heartbeat (float-time))
    (setq clatter--resume-timer
          (run-at-time clatter-ping-interval clatter-ping-interval
                       #'clatter--resume-heartbeat))))

(defun clatter--stop-resume-detector ()
  "Stop the suspend/resume heartbeat detector if running."
  (when clatter--resume-timer
    (cancel-timer clatter--resume-timer)
    (setq clatter--resume-timer nil)))

;; --- Hooks ---

(defvar clatter-connect-hook nil
  "Hook run after successfully connecting (001 received).
Called with NETWORK-ID.")

(defvar clatter-disconnect-hook nil
  "Hook run after disconnecting.
Called with NETWORK-ID and EVENT string.")

(defvar clatter-reconnect-hook nil
  "Hook run when scheduling a reconnection attempt.
Called with NETWORK-ID, DELAY (seconds), and ATTEMPT (integer).")

;; --- Labeled Responses ---

(defun clatter-generate-label (conn)
  "Generate a unique label for CONN."
  (format "clatter%d" (cl-incf (clatter-connection-label-counter conn))))

(defun clatter-send-labeled (conn command &optional callback)
  "Send COMMAND on CONN with a label tag for labeled-response.
CALLBACK is called with the response message when received.
Returns the label used, or nil if labeled-response not available."
  (if (member "labeled-response" (clatter-connection-cap-enabled conn))
      (let ((label (clatter-generate-label conn)))
        (when callback
          (puthash label callback (clatter-connection-pending-labels conn)))
        (clatter-send conn (format "@label=%s %s" label command))
        label)
    (progn
      (clatter-send conn command)
      nil)))

(provide 'clatter-connection)

;;; clatter-connection.el ends here
