;;; clatter-soju.el --- Soju bouncer-networks fan-out -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Soju (and compatible) bouncer support via the IRCv3
;; `soju.im/bouncer-networks' extension.
;;
;; A "control" network entry configured with `:bouncer t' and the bare
;; bouncer `:username' (no \"/network\") connects to the bouncer, negotiates
;; the cap, and runs `BOUNCER LISTNETWORKS'.  The bouncer replies with a
;; `soju.im/bouncer-networks' batch of `BOUNCER NETWORK <netid> <attributes>'
;; entries.  This module then spawns a child connection per upstream network
;; -- each a normal clatter network with its own channels and chathistory --
;; by calling `clatter-connect' with the \"/network\" username form
;; (\"<bouncer-user>/<network-name>\"), which Soju routes to the chosen
;; upstream without any `BOUNCER BIND'.  Children carry a \"/network\"
;; username, so they are not control connections and never re-list (no
;; loops).
;;
;; Loading this file has no side effects; hooks are installed only when
;; `clatter-soju-enable' runs (wired into `clatter-setup' behind
;; `clatter-soju-enabled').  The hooks are no-ops on connections that do not
;; negotiate bouncer-networks.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-connection)
(require 'clatter-handlers)
(require 'clatter-protocol)

;; --- Configuration ---

(defcustom clatter-soju-enabled t
  "Enable Soju bouncer-networks fan-out.
When non-nil, `clatter-setup' installs the hooks that turn a
`:bouncer t' control connection (one with a bare bouncer `:username')
into a set of child connections, one per upstream network advertised
by the bouncer.  The hooks are inert on ordinary (non-bouncer)
connections and on single-network bouncer connections."
  :type 'boolean
  :group 'clatter)

;; --- Control-connection state ---

;; Each control connection is identified by its clatter network-id.  Its
;; value is a plist: (:networks HASH netid->attrs-alist :children HASH
;; netid->child-network-id).
(defvar clatter-soju--controls (make-hash-table :test 'equal)
  "Map of control connection network-id to its fan-out state.")

(defun clatter-soju--state (network-id)
  "Return (creating if needed) the fan-out state for control NETWORK-ID."
  (or (gethash network-id clatter-soju--controls)
      (let ((state (list :networks (make-hash-table :test 'equal)
                         :children (make-hash-table :test 'equal))))
        (puthash network-id state clatter-soju--controls)
        state)))

(defun clatter-soju--control-config (conn)
  "Return the effective config plist for the control connection CONN.
Prefer the live process config (honoring `clatter-connect' overrides);
fall back to the saved `clatter-networks' entry."
  (or (and-let* ((proc (clatter-connection-process conn))
                 ((processp proc)))
        (process-get proc :clatter-config))
      (cdr (assoc (clatter-connection-network-id conn)
                  clatter-networks #'equal))))

;; --- BOUNCER NETWORK parsing ---

(defun clatter-soju--parse-attrs (attrs-string)
  "Parse a BOUNCER NETWORK ATTRS-STRING into an alist of (key . value).
Attributes use IRCv3 message-tag format (`name=libera;state=connected'),
so `clatter-parse-tags' handles the `;' splitting and `\\s'/`\\:' unescaping."
  (when attrs-string
    (clatter-parse-tags attrs-string)))

(defun clatter-soju--on-bouncer (conn subcommand params)
  "Handle a BOUNCER command on CONN.
SUBCOMMAND is the BOUNCER subcommand; PARAMS is the list after it.
Only `NETWORK' notifications are interesting: they carry
`<netid> <attributes>' (or `<netid> *' for a removal)."
  (when (string-equal subcommand "NETWORK")
    (let ((netid (nth 0 params))
          (attrs (nth 1 params))
          (state (clatter-soju--state (clatter-connection-network-id conn))))
      (if (string-equal attrs "*")
          ;; Removal notification: drop the network and (best-effort) its child.
          (remhash netid (plist-get state :networks))
        (puthash netid (clatter-soju--parse-attrs attrs)
                 (plist-get state :networks))))))

;; --- Fan-out ---

(defun clatter-soju--child-id (netid attrs)
  "Return the clatter network-id to use for a child bound to NETID.
Prefer the network's `name' attribute; fall back to the netid."
  (or (cdr (assoc "name" attrs #'string=)) netid))

(defun clatter-soju--fan-out-args (control-id control-config attrs child-id)
  "Return the `clatter-connect' keyword plist for a child of CONTROL-CONFIG.
CONTROL-ID is the clatter network-id of the control connection, used to
resolve the bouncer password via auth-source when CONTROL-CONFIG has no
explicit `:password'.  ATTRS is the network's parsed attributes; CHILD-ID
the clatter network-id to use (the network's `name').  The child connects
to the same bouncer endpoint as the control connection and routes to the
upstream via the \"<bouncer-user>/<network-name>\" username form, so Soju
binds it to that network without any `BOUNCER BIND'.  It inherits `:bouncer
t' (skip NickServ/reclaim) and the bouncer credentials, and deliberately
omits `:autojoin' (Soju replays saved JOINs upstream-side)."
  (let* ((bouncer-user (or (plist-get control-config :username)
                           (plist-get control-config :nick)))
         (nick (or (cdr (assoc "nickname" attrs #'string=))
                   (plist-get control-config :nick)))
         (args (list :server (plist-get control-config :server)
                     :tls (plist-get control-config :tls)
                     :nick nick
                     :username (format "%s/%s" bouncer-user child-id)
                     :bouncer t)))
    (dolist (key '(:port :sasl :client-cert :proxy :tor))
      (when (plist-member control-config key)
        (setq args (plist-put args key (plist-get control-config key)))))
    ;; A bouncer child authenticates to the bouncer with the control's
    ;; credentials, so resolve the bouncer password the same way the
    ;; control does: explicit `:password', then auth-source keyed on the
    ;; *control* network-id/server/user.  The child's own lookup would use
    ;; the child network-id and nick, which miss the bouncer creds and fail
    ;; the SASL password precheck for children whose upstream name differs
    ;; from the bouncer identity (e.g. an OFTC child of a bouncer whose
    ;; auth-source entry is stored under the control network-id).
    (let ((password (or (plist-get control-config :password)
                        (clatter-get-password control-id control-config))))
      (when password
        (setq args (plist-put args :password password))))
    args))

(defun clatter-soju--child-live-p (child-id)
  "Return non-nil if CHILD-ID has a live connection process.
A child whose server buffer was killed is explicitly disconnected: its
process is gone, so this returns nil and the control's next fan-out
re-spawns it (bringing it back when the control reconnects)."
  (when-let* ((conn (clatter-get-connection child-id))
             (proc (clatter-connection-process conn)))
    (process-live-p proc)))

(defun clatter-soju--fan-out (conn)
  "Spawn child connections for every network stashed for CONN.
Idempotent across reconnects: a network whose child connection is still
live is skipped; one whose child was disconnected (e.g. its server buffer
killed) is re-spawned, so reconnecting the control brings dead children
back.  `clatter-connect' reuses the child's existing connection struct and
cancels any pending reconnect, so re-spawning a dead child is safe."
  (let* ((network-id (clatter-connection-network-id conn))
         (state (clatter-soju--state network-id))
         (networks (plist-get state :networks))
         (children (plist-get state :children))
         (control-config (clatter-soju--control-config conn)))
    (unless control-config
      (clatter--debug "soju: no control config for %s; skipping fan-out"
                      network-id))
    (when control-config
      (maphash
       (lambda (netid attrs)
         (let ((existing (gethash netid children)))
           (if (and existing (clatter-soju--child-live-p existing))
               (clatter--debug "soju: %s still connected via %s; skipping"
                               netid existing)
             (let* ((child-id (clatter-soju--child-id netid attrs))
                    (args (clatter-soju--fan-out-args
                           network-id control-config attrs child-id)))
               (puthash netid child-id children)
               (clatter--debug "soju: fanning out %s -> %s%s"
                               netid child-id
                               (if existing " (reconnecting)" ""))
               (apply #'clatter-connect child-id args)))))
       networks))))

(defun clatter-soju--on-batch-complete (conn batch-type _target _messages)
  "Fan out when CONN completes a `soju.im/bouncer-networks' batch."
  (when (string-equal batch-type "soju.im/bouncer-networks")
    (clatter-soju--fan-out conn)))

(defun clatter-soju--children-of (network-id)
  "Return the child network-ids spawned by control NETWORK-ID."
  (when-let* ((state (gethash network-id clatter-soju--controls)))
    (hash-table-values (plist-get state :children))))

(defun clatter-soju--on-disconnect (network-id reason)
  "Disconnect NETWORK-ID's children after an explicit control disconnect.
A user-initiated control QUIT takes the whole bouncer session down, so
its fanned-out children must go too.  Transient control drops (server
close, half-open kill) leave live children alone: they are independent
connections, and the control's reconnect fan-out re-spawns only dead
ones.  REASON is the disconnect hook's reason string."
  (when (and (string-prefix-p "Disconnected" reason)
             (gethash network-id clatter-soju--controls))
    (dolist (child (clatter-soju--children-of network-id))
      (when (and (clatter-get-connection child)
                 (clatter-soju--child-live-p child))
        (clatter-disconnect child)))))

;; --- Enable / disable ---

(defun clatter-soju-enable ()
  "Install the Soju bouncer-networks hooks."
  (add-hook 'clatter-bouncer-hook #'clatter-soju--on-bouncer)
  (add-hook 'clatter-batch-complete-hook #'clatter-soju--on-batch-complete)
  (add-hook 'clatter-disconnect-hook #'clatter-soju--on-disconnect))

(defun clatter-soju-disable ()
  "Remove the Soju bouncer-networks hooks."
  (remove-hook 'clatter-bouncer-hook #'clatter-soju--on-bouncer)
  (remove-hook 'clatter-batch-complete-hook #'clatter-soju--on-batch-complete)
  (remove-hook 'clatter-disconnect-hook #'clatter-soju--on-disconnect))

(provide 'clatter-soju)

;;; clatter-soju.el ends here