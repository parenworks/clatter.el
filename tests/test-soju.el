;;; test-soju.el --- Tests for clatter-soju.el and Soju SASL/BIND -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-cap)
(require 'clatter-soju)

(defun clatter-soju-test--reset ()
  "Clear the Soju fan-out control state between tests."
  (clrhash clatter-soju--controls))

;; --- Attribute parsing ---

(ert-deftest clatter-soju-test-parse-attrs ()
  "BOUNCER NETWORK attributes parse as message-tag pairs with unescaping."
  (let ((attrs (clatter-soju--parse-attrs
                "name=My\\sAwesome\\sNetwork;state=connected;host=irc.libera.chat")))
    (should (equal (cdr (assoc "name" attrs #'string=))
                   "My Awesome Network"))
    (should (equal (cdr (assoc "state" attrs #'string=)) "connected"))
    (should (equal (cdr (assoc "host" attrs #'string=)) "irc.libera.chat"))))

(ert-deftest clatter-soju-test-child-id-prefers-name ()
  "The child network-id prefers the network `name' attribute."
  (should (equal (clatter-soju--child-id
                  "42" '(("name" . "libera") ("state" . "connected")))
                 "libera"))
  (should (equal (clatter-soju--child-id "42" '(("state" . "connected")))
                 "42")))

;; --- fan-out args ---

(ert-deftest clatter-soju-test-fan-out-args ()
  "Child args route via \"user/network\" and inherit bouncer credentials."
  (let ((control-config '(:server "soju.example" :port 6697 :tls t
                          :nick "me" :username "sojuuser" :password "secret"
                          :sasl plain :bouncer t))
        (attrs (clatter-soju--parse-attrs
                "name=libera;state=connected;nickname=trev")))
    (let ((args (clatter-soju--fan-out-args
                 "soju" control-config attrs "libera")))
      (should (equal (plist-get args :server) "soju.example"))
      (should (equal (plist-get args :port) 6697))
      (should (eq (plist-get args :tls) t))
      ;; The upstream nick comes from the network's `nickname' attribute.
      (should (equal (plist-get args :nick) "trev"))
      ;; Routing is via the "<bouncer-user>/<network-name>" username form;
      ;; no BOUNCER BIND, so no :bouncer-bind keyword.
      (should (equal (plist-get args :username) "sojuuser/libera"))
      (should-not (plist-member args :bouncer-bind))
      (should (eq (plist-get args :bouncer) t))
      ;; Bouncer credentials are inherited so the child authenticates as the
      ;; same bouncer user.
      (should (equal (plist-get args :password) "secret"))
      (should (eq (plist-get args :sasl) 'plain))
      ;; Children must not auto-join (Soju replays saved JOINs upstream-side).
      (should-not (plist-member args :autojoin)))))

(ert-deftest clatter-soju-test-fan-out-args-nick-fallback ()
  "Without a `nickname' attribute the child falls back to the control nick."
  (let ((control-config '(:server "soju.example" :tls t :nick "me"
                          :username "sojuuser" :password "secret"))
        (attrs (clatter-soju--parse-attrs "name=libera;state=connected")))
    (let ((args (clatter-soju--fan-out-args
                 "soju" control-config attrs "libera")))
      (should (equal (plist-get args :nick) "me"))
      (should (equal (plist-get args :username) "sojuuser/libera")))))

(ert-deftest clatter-soju-test-fan-out-args-resolves-control-password ()
  "A child inherits the bouncer password resolved via auth-source.
The control has no explicit `:password'; the child must still carry the
bouncer password so its SASL precheck and PASS registration succeed.
The lookup is keyed on the *control* network-id, not the child's, so a
child whose upstream name differs from the bouncer creds (e.g. oftc)
still resolves the right password."
  (let ((control-config '(:server "soju.example" :tls t :nick "me"
                          :username "sojuuser" :sasl plain :bouncer t))
        (attrs (clatter-soju--parse-attrs "name=oftc;state=connected")))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest keys)
                 ;; Only the control identity matches; the child identity
                 ;; would miss, which is exactly the bug this fixes.
                 (when (equal (plist-get keys :host) "soju")
                   (list (list :secret (lambda () "bouncer-secret")))))))
      (let ((args (clatter-soju--fan-out-args
                   "soju" control-config attrs "oftc")))
        (should (equal (plist-get args :password) "bouncer-secret"))
        (should (eq (plist-get args :sasl) 'plain))))))

;; --- BOUNCER NETWORK stashing + fan-out ---

(ert-deftest clatter-soju-test-on-bouncer-stashes-network ()
  "BOUNCER NETWORK notifications are stashed per control connection."
  (clatter-soju-test--reset)
  (let ((conn (clatter-test-make-connection "soju" "me")))
    (clatter-soju--on-bouncer conn "NETWORK"
                              '("42" "name=libera;state=connected;nickname=trev"))
    (clatter-soju--on-bouncer conn "NETWORK" '("43" "name=oftc;state=disconnected"))
    (let ((state (gethash "soju" clatter-soju--controls)))
      (should state)
      (should (equal (gethash "42" (plist-get state :networks))
                     '(("name" . "libera") ("state" . "connected")
                       ("nickname" . "trev"))))
      (should (equal (gethash "43" (plist-get state :networks))
                     '(("name" . "oftc") ("state" . "disconnected"))))))

  ;; Removal notification (`*') drops the network.
  (let ((conn (clatter-test-make-connection "soju" "me")))
    (clatter-soju--on-bouncer conn "NETWORK" '("42" "name=libera"))
    (clatter-soju--on-bouncer conn "NETWORK" '("42" "*"))
    (let ((state (gethash "soju" clatter-soju--controls)))
      (should-not (gethash "42" (plist-get state :networks))))))

(ert-deftest clatter-soju-test-fan-out-spawns-bound-children ()
  "A bouncer-networks batch spawns one child per stashed network."
  (clatter-soju-test--reset)
  (let ((clatter-networks
         '(("soju" :server "soju.example" :port 6697 :tls t
            :nick "me" :username "sojuuser" :password "secret"
            :sasl plain :bouncer t)))
        spawned)
    (cl-letf (((symbol-function 'clatter-connect)
               (lambda (network-id &rest args)
                 (push (cons network-id args) spawned))))
      (let ((conn (clatter-test-make-connection "soju" "me")))
        (setf (clatter-connection-process conn)
              (make-pipe-process :name "soju-ctrl" :noquery t))
        (process-put (clatter-connection-process conn) :clatter-config
                     (cdr (assoc "soju" clatter-networks #'equal)))
        (clatter-soju--on-bouncer conn "NETWORK"
                                  '("42" "name=libera;state=connected;nickname=trev"))
        (clatter-soju--on-bouncer conn "NETWORK"
                                  '("43" "name=oftc;state=disconnected"))
        (clatter-soju--on-batch-complete conn "soju.im/bouncer-networks" nil nil)
        (delete-process (clatter-connection-process conn))))
    (should (= 2 (length spawned)))
    (dolist (entry spawned)
      (cl-destructuring-bind (id . args) entry
        (should (member id '("libera" "oftc")))
        ;; Children route via "<bouncer-user>/<network-name>"; no BIND keyword.
        (should (equal (plist-get args :username)
                       (format "sojuuser/%s" id)))
        (should-not (plist-member args :bouncer-bind))
        (should (eq (plist-get args :bouncer) t))
        (should-not (plist-member args :autojoin))
        (should (equal (plist-get args :server) "soju.example"))))))

(ert-deftest clatter-soju-test-fan-out-is-idempotent ()
  "Re-running the fan-out does not respawn a still-live child."
  (clatter-soju-test--reset)
  (let ((clatter-networks
         '(("soju" :server "soju.example" :tls t :nick "me"
            :username "sojuuser" :password "secret" :bouncer t)))
        (spawn-count 0))
    (cl-letf (((symbol-function 'clatter-connect)
               (lambda (&rest _) (cl-incf spawn-count)))
              ;; The child spawned by run 1 is still live, so run 2 skips it.
              ((symbol-function 'clatter-soju--child-live-p) (lambda (_) t)))
      (dolist (run '(1 2))
        (let ((conn (clatter-test-make-connection "soju" "me")))
          (setf (clatter-connection-process conn)
                (make-pipe-process :name (format "soju-ctrl-%d" run) :noquery t))
          (process-put (clatter-connection-process conn) :clatter-config
                       (cdr (assoc "soju" clatter-networks #'equal)))
          (clatter-soju--on-bouncer conn "NETWORK"
                                   '("42" "name=libera;state=connected"))
          (clatter-soju--on-batch-complete conn "soju.im/bouncer-networks" nil nil)
          (delete-process (clatter-connection-process conn)))))
    ;; Only the first run spawns; the second sees the child still live.
    (should (= 1 spawn-count))))

(ert-deftest clatter-soju-test-fan-out-respawns-dead-child ()
  "A child whose connection died (e.g. its buffer was killed) is re-spawned."
  (clatter-soju-test--reset)
  (let ((clatter-networks
         '(("soju" :server "soju.example" :tls t :nick "me"
            :username "sojuuser" :password "secret" :bouncer t)))
        (spawn-count 0))
    (cl-letf (((symbol-function 'clatter-connect)
               (lambda (&rest _) (cl-incf spawn-count)))
              ;; Child never live: simulates a killed/disconnected child.
              ((symbol-function 'clatter-soju--child-live-p) (lambda (_) nil)))
      (dolist (run '(1 2))
        (let ((conn (clatter-test-make-connection "soju" "me")))
          (setf (clatter-connection-process conn)
                (make-pipe-process :name (format "soju-ctrl-%d" run) :noquery t))
          (process-put (clatter-connection-process conn) :clatter-config
                       (cdr (assoc "soju" clatter-networks #'equal)))
          (clatter-soju--on-bouncer conn "NETWORK"
                                   '("42" "name=libera;state=connected"))
          (clatter-soju--on-batch-complete conn "soju.im/bouncer-networks" nil nil)
          (delete-process (clatter-connection-process conn)))))
    ;; Both runs spawn because the child is never live.
    (should (= 2 spawn-count))))

(ert-deftest clatter-soju-test-child-live-p ()
  "`clatter-soju--child-live-p' tracks the child's process liveness."
  (clatter-soju-test--reset)
  ;; No connection at all -> not live.
  (should-not (clatter-soju--child-live-p "absent"))
  ;; Live process -> live.
  (let ((conn (clatter-test-make-connection "libera" "trev"))
        proc)
    (unwind-protect
        (progn
          (setq proc (make-pipe-process :name "libera-proc" :noquery t))
          (setf (clatter-connection-process conn) proc)
          (puthash "libera" conn clatter-connections)
          (should (clatter-soju--child-live-p "libera"))
          ;; Dead process -> not live (buffer was killed / disconnected).
          (delete-process proc)
          (should-not (clatter-soju--child-live-p "libera")))
      (when (process-live-p proc) (delete-process proc))
      (remhash "libera" clatter-connections))))

(ert-deftest clatter-soju-test-batch-complete-ignores-other-types ()
  "Non bouncer-networks batches do not trigger fan-out."
  (clatter-soju-test--reset)
  (let ((spawn-count 0))
    (cl-letf (((symbol-function 'clatter-connect) (lambda (&rest _) (cl-incf spawn-count)))
              ((symbol-function 'clatter-soju--fan-out)
               (lambda (&rest _) (cl-incf spawn-count))))
      (let ((conn (clatter-test-make-connection "soju" "me")))
        (clatter-soju--on-batch-complete conn "chathistory" nil nil)
        (clatter-soju--on-batch-complete conn "net.example/whatever" nil nil)))
    (should (= 0 spawn-count))))

;; --- Control-vs-child predicate ---

(ert-deftest clatter-test-bouncer-networks-p ()
  "`:bouncer t' with a bare username is a control connection."
  ;; Bare username -> control (fan-out).
  (should (clatter-bouncer-networks-p
           '(:bouncer t :username "sojuuser" :nick "me")))
  ;; No :username: the nick is the effective username; still control if bare.
  (should (clatter-bouncer-networks-p '(:bouncer t :nick "me")))
  ;; A per-client "@client" suffix is still bouncer-networks mode (no "/").
  (should (clatter-bouncer-networks-p
           '(:bouncer t :username "sojuuser@laptop" :nick "me")))
  ;; A "/network" username is a single-network bouncer connection.
  (should-not (clatter-bouncer-networks-p
               '(:bouncer t :username "sojuuser/libera" :nick "me")))
  ;; A child fanned out with "<user>/<name>" is therefore not a control conn.
  (should-not (clatter-bouncer-networks-p
               '(:bouncer t :username "sojuuser/libera" :nick "trev")))
  ;; No :bouncer flag -> never a control connection.
  (should-not (clatter-bouncer-networks-p
               '(:username "sojuuser" :nick "me"))))

(ert-deftest clatter-test-cap-requests-soju-cap-for-control-only ()
  "Only control bouncer connections request soju.im/bouncer-networks."
  (dolist (case '((:control "soju-ctrl"
                   (:bouncer t :username "sojuuser" :nick "me") t)
                  (:child "soju-child"
                   (:bouncer t :username "sojuuser/libera" :nick "trev") nil)
                  (:per-network "soju-net"
                   (:bouncer t :username "sojuuser/libera" :nick "trev") nil)))
    (cl-destructuring-bind (_ net-id config expect) case
      (clatter-test-with-mock-send
        (let ((proc (make-pipe-process :name net-id :noquery t))
              (conn (clatter-test-make-connection net-id "me")))
          (unwind-protect
              (progn
                (setf (clatter-connection-process conn) proc)
                (process-put proc :clatter-config config)
                (cl-letf (((symbol-function 'clatter-sts-check-cap)
                           (lambda (&rest _) nil)))
                  (clatter-cap--handle-ls
                   conn "server-time batch sasl=PLAIN soju.im/bouncer-networks soju.im/bouncer-networks-notify")))
                (let ((req (clatter-test-sent-matching "CAP REQ")))
                  (if expect
                      (should (and req
                                   (string-match-p "soju.im/bouncer-networks"
                                                   req)))
                    ;; Children/per-network never request the soju caps.
                    (should-not (and req
                                     (string-match-p "soju.im/bouncer-networks"
                                                     req)))))
            (delete-process proc)))))))

;; --- SASL PLAIN authcid ---

(ert-deftest clatter-test-sasl-plain-uses-username-as-authcid ()
  "SASL PLAIN authenticates with :username when set (Soju bare-username auth)."
  (clatter-test-with-mock-send
    (let ((proc (make-pipe-process :name "soju-sasl" :noquery t))
          (conn (clatter-test-make-connection "soju" "mynick")))
      (unwind-protect
          (progn
            (setf (clatter-connection-process conn) proc)
            (process-put proc :clatter-config
                         '(:server "soju.example" :nick "mynick"
                           :username "sojuuser" :password "secret"))
            (let ((clatter-networks
                   '(("soju" :server "soju.example" :nick "mynick"
                      :username "sojuuser" :password "secret"))))
              (cl-letf (((symbol-function 'auth-source-search)
                         (lambda (&rest _) nil)))
                (clatter-cap--sasl-plain-authenticate conn)))
            (let* ((line (clatter-test-sent-matching "AUTHENTICATE"))
                   (b64 (cadr (split-string line)))
                   (decoded (base64-decode-string b64)))
              (should line)
              ;; PLAIN: \0authcid\0password -- authcid is the bouncer username.
              (should (string-match-p "\x00sojuuser\x00secret" decoded))
              (should-not (string-match-p "\x00mynick\x00" decoded))))
        (delete-process proc)))))

(ert-deftest clatter-test-sasl-plain-falls-back-to-nick-without-username ()
  "SASL PLAIN uses the nick when :username is not set (no regression)."
  (clatter-test-with-mock-send
    (let ((proc (make-pipe-process :name "plain-nick" :noquery t))
          (conn (clatter-test-make-connection "libera" "mynick")))
      (unwind-protect
          (progn
            (setf (clatter-connection-process conn) proc)
            (process-put proc :clatter-config '(:nick "mynick" :password "secret"))
            (let ((clatter-networks
                   '(("libera" :server "irc.libera.chat" :nick "mynick"
                      :password "secret"))))
              (cl-letf (((symbol-function 'auth-source-search)
                         (lambda (&rest _) nil)))
                (clatter-cap--sasl-plain-authenticate conn)))
            (let* ((line (clatter-test-sent-matching "AUTHENTICATE"))
                   (b64 (cadr (split-string line)))
                   (decoded (base64-decode-string b64)))
              (should (string-match-p "\x00mynick\x00secret" decoded))))
        (delete-process proc)))))


;; --- Control disconnect tears down children ---

(ert-deftest clatter-soju-test-explicit-control-disconnect-kills-children ()
  "An explicit control QUIT disconnects its live children too.
A control disconnect surfacing a `Disconnected...' reason takes the
whole bouncer session down; leaving children connected is the reported
`clatter-disconnect does nothing' bug."
  (clatter-soju-test--reset)
  (let (disconnected)
    (cl-letf (((symbol-function 'clatter-disconnect)
               (lambda (network-id &rest _)
                 (push network-id disconnected))))
      (let ((conn (clatter-test-make-connection "soju" "me"))
            (clatter-connections
             (let ((table (make-hash-table :test 'equal)))
               (puthash "libera" (clatter-test-make-connection "libera" "t")
                        table)
               table)))
        (setf (clatter-connection-process conn)
              (make-pipe-process :name "soju-ctrl" :noquery t))
        (clatter-soju--on-bouncer conn "NETWORK"
                                  '("42" "name=libera;state=connected"))
        (puthash "42" "libera"
                 (plist-get (gethash "soju" clatter-soju--controls) :children))
        (let ((clatter-disconnect-hook
               (list #'clatter-soju--on-disconnect)))
          (cl-letf (((symbol-function 'clatter-soju--child-live-p)
                     (lambda (_) t)))
            (run-hook-with-args 'clatter-disconnect-hook
                                "soju" "Disconnected by user")))
        (delete-process (clatter-connection-process conn))))
    (should (equal disconnected '("libera")))))

(ert-deftest clatter-soju-test-transient-control-drop-spares-children ()
  "A server-side control drop (non-Disconnected reason) leaves children.
Children are independent connections; the control's reconnect fan-out
re-spawns only dead ones."
  (clatter-soju-test--reset)
  (let (disconnected)
    (cl-letf (((symbol-function 'clatter-disconnect)
               (lambda (network-id &rest _)
                 (push network-id disconnected))))
      (let ((conn (clatter-test-make-connection "soju" "me")))
        (setf (clatter-connection-process conn)
              (make-pipe-process :name "soju-ctrl2" :noquery t))
        (clatter-soju--on-bouncer conn "NETWORK"
                                  '("42" "name=libera;state=connected"))
        (puthash "42" "libera"
                 (plist-get (gethash "soju" clatter-soju--controls) :children))
        (let ((clatter-disconnect-hook
               (list #'clatter-soju--on-disconnect)))
          (cl-letf (((symbol-function 'clatter-soju--child-live-p)
                     (lambda (_) t)))
            ;; Server-side control drop: children stay.
            (run-hook-with-args 'clatter-disconnect-hook
                                "soju" "Connection closed by remote peer")
            ;; A child's own explicit disconnect is not a control teardown.
            (run-hook-with-args 'clatter-disconnect-hook
                                "libera" "Disconnected by user")))
        (delete-process (clatter-connection-process conn))))
    (should-not disconnected)))

(ert-deftest clatter-soju-test-enable-registers-disconnect-hook ()
  "`clatter-soju-enable' installs the teardown hook; disable removes it."
  (unwind-protect
      (progn
        (clatter-soju-disable)
        (should-not (memq #'clatter-soju--on-disconnect
                         (default-value 'clatter-disconnect-hook)))
        (clatter-soju-enable)
        (should (memq #'clatter-soju--on-disconnect
                      (default-value 'clatter-disconnect-hook))))
    (clatter-soju-disable)))

(provide 'test-soju)

;;; test-soju.el ends here
