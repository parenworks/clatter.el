;;; test-ui.el --- Tests for clatter-ui.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-actions)
(require 'clatter-commands)
(require 'clatter-nicklist)
(require 'clatter-pals)

(ert-deftest clatter-buffer-name-channel-style-qualifies-server-buffers ()
  "Channel-style server buffers retain their network names."
  (let ((clatter-buffer-name-style 'channel))
    (should (equal (clatter-server-buffer-name "libera")
                   "*libera/*server**"))
    (should (equal (clatter-server-buffer-name "ergo")
                   "*ergo/*server**"))
    (should (equal (clatter-buffer-name "libera" "#emacs") "#emacs"))))

(ert-deftest clatter-navigation-orders-messages-and-interactive-items ()
  "Navigation visits a message, its link, then the following message."
  (with-temp-buffer
    (let ((clatter-nick-column-width 8))
      (insert (clatter--format-nick-column "<alice>" nil "alice")
              " see "
              (clatter-hl-urls-in-string "https://example.com")
              "\n"
              (clatter--format-nick-column "<bob>" nil "bob")
              " hello\n"))
    (goto-char (point-min))
    (clatter-next-item)
    (should (equal (get-text-property (point) 'clatter-sender) "alice"))
    (should (looking-at-p "<alice>"))
    (clatter-next-item)
    (should (button-at (point)))
    (clatter-next-item)
    (should (equal (get-text-property (point) 'clatter-sender) "bob"))
    (clatter-previous-item)
    (should (button-at (point)))
    (clatter-previous-item)
    (should (looking-at-p "<alice>"))))

(ert-deftest clatter-navigation-skips-invisible-items ()
  "Navigation does not stop at suppressed messages or their buttons."
  (with-temp-buffer
    (insert (propertize "hidden"
                        'clatter-navigation-target 'message
                        'invisible t)
            " "
            (propertize "hidden-link"
                        'button '(t)
                        'category 'default-button
                        'invisible t)
            "\nvisible")
    (put-text-property (- (point-max) 7) (point-max)
                       'clatter-navigation-target 'message)
    (goto-char (point-min))
    (clatter-next-item)
    (should (looking-at-p "visible"))))

(ert-deftest clatter-navigation-includes-follow-link-text ()
  "Navigation visits interactive text that is not a standard button."
  (with-temp-buffer
    (insert "x " (propertize "context" 'follow-link t) " y")
    (goto-char (point-min))
    (clatter-next-item)
    (should (looking-at-p "context"))))

(ert-deftest clatter-reply-context-is-a-navigable-button ()
  "Reply context is a standard button that jumps to its message."
  (let ((conn (clatter-test-make-connection "testnet" "me")))
    (unwind-protect
        (dolist (order '(oldest-first newest-first))
          (with-temp-buffer
            (let ((clatter-message-order order))
              (clatter-mode)
              (setq-local clatter--network "testnet")
              (setq-local clatter--target "#test")
              (clatter-ui-setup-buffer (current-buffer))
              (clatter-insert-privmsg
               (current-buffer) "alice"
               (propertize "original" 'clatter-msgid "msg-1") conn)
              (let ((buffer (current-buffer)))
                ;; Process filters render while their connection buffer is current.
                (with-temp-buffer
                  (clatter-insert-privmsg
                   buffer "bob"
                   (propertize "reply" 'clatter-reply-to "msg-1") conn)))
              (let* ((button (next-button (point-min) t))
                     (position (and button (button-start button))))
                (should button)
                (should (equal (button-get button 'reply-to) "msg-1"))
                (goto-char position)
                (push-button)
                (should (equal (get-text-property (point) 'clatter-msgid)
                               "msg-1"))))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-reply-selects-message-at-line-boundary ()
  "Replying at line start sends the current message's ID."
  (let ((conn (clatter-test-make-connection "testnet" "me")))
    (unwind-protect
        (dolist (order '(oldest-first newest-first))
          (with-temp-buffer
            (let ((clatter-message-order order))
              (clatter-mode)
              (setq-local clatter--network "testnet")
              (setq-local clatter--target "#test")
              (clatter-ui-setup-buffer (current-buffer))
              (dolist (message '(("alice" "first" "msg-1")
                                 ("bob" "second" "msg-2")
                                 ("carol" "third" "msg-3")))
                (clatter-insert-privmsg
                 (current-buffer) (nth 0 message)
                 (propertize (nth 1 message)
                             'clatter-msgid (nth 2 message))
                 conn))
              (goto-char (clatter--find-message-position-by-msgid
                          (current-buffer) "msg-2"))
              (clatter-action-reply)
              (should (equal (clatter--get-input) "/reply bob: "))
              (should (equal (get-text-property
                              (overlay-start mouse-secondary-overlay)
                              'clatter-msgid)
                             "msg-2"))
              (clatter-test-with-mock-send
                (clatter-cmd-reply "answer")
                (should (equal (clatter-test-last-sent)
                               "@+reply=msg-2 PRIVMSG #test answer"))))))
      (when (overlayp mouse-secondary-overlay)
        (delete-overlay mouse-secondary-overlay))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-tab-and-backtab-preserve-input-behavior ()
  "TAB completes and BACKTAB stays undefined while point is in input."
  (dolist (order '(oldest-first newest-first))
    (with-temp-buffer
      (let ((clatter-message-order order)
            expected
            (completion-calls 0)
            (undefined-calls 0))
        (if (eq order 'oldest-first)
            (progn
              (insert (propertize "older" 'clatter-navigation-target 'message)
                      " "
                      (propertize "newer" 'clatter-navigation-target 'message)
                      "\n")
              (setq expected 7)
              (setq-local clatter--messages-marker (copy-marker (point-min)))
              (setq-local clatter--input-marker (copy-marker (point)))
              (insert "input"))
          (setq-local clatter--input-marker (copy-marker (point-min)))
          (insert "input\n")
          (setq-local clatter--messages-marker (copy-marker (point)))
          (setq expected (point))
          (insert (propertize "newer" 'clatter-navigation-target 'message)
                  " "
                  (propertize "older" 'clatter-navigation-target 'message)))
        (cl-letf (((symbol-function 'completion-at-point)
                   (lambda ()
                     (cl-incf completion-calls)))
                  ((symbol-function 'undefined)
                   (lambda ()
                     (interactive)
                     (cl-incf undefined-calls))))
          (goto-char clatter--input-marker)
          (let ((origin (point)))
            (clatter-tab)
            (should (= (point) origin))
            (clatter-backtab)
            (should (= (point) origin)))
          (should (= completion-calls 1))
          (should (= undefined-calls 1)))
        ;; The spatially adjacent history command returns to input.
        (goto-char expected)
        (funcall (if (eq order 'oldest-first)
                     #'clatter-tab
                   #'clatter-backtab))
        (should (clatter-in-input-p))))))
(require 'clatter-track)
(require 'clatter-notify)

;; --- Automatic buffer display ---

(defmacro clatter-test-with-ui-connection (conn &rest body)
  "Run BODY with CONN and remove its clatter buffers afterwards."
  (declare (indent 1))
  `(let ((initial-buffers clatter--buffer-alist)
         (,conn (clatter-test-make-connection)))
     (unwind-protect
         (progn ,@body)
       (dolist (entry clatter--buffer-alist)
         (when (and (not (memq entry initial-buffers))
                    (buffer-live-p (cdr entry)))
           (kill-buffer (cdr entry))))
       (setq clatter--buffer-alist initial-buffers)
       (clatter-test-cleanup))))

(ert-deftest clatter-test-join-display-can-be-disabled ()
  "Self JOIN still creates its buffer when automatic display is disabled."
  (let ((clatter-display-on-join nil)
        (displayed nil))
    (clatter-test-with-ui-connection conn
      (clatter-test-with-mock-send
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (&rest _) (setq displayed t))))
          (clatter-ui--on-join conn '("testnick" "user" "host") "#quiet" nil nil)))
      (should (clatter-get-buffer "testnet" "#quiet"))
      (should-not displayed))))

(ert-deftest clatter-test-join-display-defaults-to-enabled ()
  "Self JOIN uses `display-buffer' by default."
  (let ((clatter-display-on-join t)
        (displayed nil))
    (clatter-test-with-ui-connection conn
      (clatter-test-with-mock-send
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buf &rest _) (setq displayed buf))))
          (clatter-ui--on-join conn '("testnick" "user" "host") "#shown" nil nil)))
      (should (bufferp displayed)))))

(ert-deftest clatter-test-welcome-display-can-be-disabled ()
  "Welcome still creates the server buffer when automatic display is disabled."
  (let ((clatter-display-on-welcome nil)
        (displayed nil))
    (clatter-test-with-ui-connection conn
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (&rest _) (setq displayed t))))
        (clatter-ui--on-welcome conn "testnick"))
      (should (clatter-get-server-buffer "testnet"))
      (should-not displayed))))

(ert-deftest clatter-test-welcome-display-defaults-to-enabled ()
  "Welcome uses `display-buffer' by default."
  (let ((clatter-display-on-welcome t)
        (displayed nil))
    (clatter-test-with-ui-connection conn
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (buf &rest _) (setq displayed buf))))
        (clatter-ui--on-welcome conn "testnick"))
      (should (bufferp displayed)))))

(ert-deftest clatter-test-received-query-display-modes ()
  "Incoming queries are buried, displayed, or popped as configured."
  (dolist (case '((bury nil nil) (buffer t nil) (pop nil t)))
    (pcase-let ((`(,mode ,expect-display ,expect-pop) case))
      (let ((clatter-receive-query-display mode)
            (displayed nil)
            (popped nil))
        (clatter-test-with-ui-connection conn
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (&rest _) (setq displayed t)))
                    ((symbol-function 'pop-to-buffer)
                     (lambda (&rest _) (setq popped t))))
            (clatter-ui--on-privmsg conn '("alice" "user" "host") "TeStNiCk"
                                     "hello" nil))
          (should (clatter-get-buffer "testnet" "alice"))
          (should (eq displayed expect-display))
          (should (eq popped expect-pop)))))))

(ert-deftest clatter-test-received-query-ctcp-action-displays-with-mixed-case-target ()
  "Received CTCP ACTION uses query display policy with a case-folded target."
  (let ((clatter-receive-query-display 'pop)
        (popped nil))
    (clatter-test-with-ui-connection conn
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (buf &rest _) (setq popped buf))))
        (clatter-dispatch-message
         conn (clatter-test-parse
               ":alice!user@host PRIVMSG TeStNiCk :\C-aACTION waves\C-a")))
      (should (eq popped (clatter-get-buffer "testnet" "alice"))))))

(ert-deftest clatter-test-rfc1459-self-echo-does-not-display-query ()
  "RFC1459-equivalent self echoes never apply received-query display policy."
  (let ((clatter-receive-query-display 'pop)
        (popped nil))
    (clatter-test-with-ui-connection conn
      (setf (clatter-connection-nick conn) "{nick")
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (&rest _) (setq popped t))))
        ;; Under RFC1459 CASEMAPPING, [NICK and {nick are the same nick.
        (clatter-ui--on-privmsg conn '("[NICK" "user" "host") "{nick"
                                 "echo" nil))
      (should (clatter-get-buffer "testnet" "[NICK"))
      (should-not popped))))
;; --- Query NOTICE routing ---

(ert-deftest clatter-test-notice-dm-routes-to-query-buffer ()
  "A query NOTICE from a user lands in that user's query buffer."
  (clatter-test-with-ui-connection conn
    ;; Pre-create the query buffer so we can assert the NOTICE lands there
    ;; rather than creating a server buffer.
    (let ((query (clatter-get-or-create-buffer "testnet" "alice")))
      (clatter-ui-setup-buffer-if-needed query)
      (clatter-ui--on-notice conn '("alice" "user" "host") "testnick" "beep" nil)
      (should (eq (clatter-get-buffer "testnet" "alice") query))
      (should (eq 'query (buffer-local-value 'clatter--buffer-type query)))
      (should (string-match-p "beep"
                              (with-current-buffer query (buffer-string))))
      ;; A query NOTICE must not create or touch the server buffer.
      (should-not (clatter-get-server-buffer "testnet")))))

(ert-deftest clatter-test-notice-dm-creates-query-buffer ()
  "A query NOTICE from a new correspondent creates its query buffer on demand."
  (clatter-test-with-ui-connection conn
    (should-not (clatter-get-buffer "testnet" "bob"))
    (clatter-ui--on-notice conn '("bob" "user" "host") "testnick" "hi" nil)
    (let ((buf (clatter-get-buffer "testnet" "bob")))
      (should buf)
      (should (eq 'query (buffer-local-value 'clatter--buffer-type buf)))
      (should-not (clatter-get-server-buffer "testnet")))))

(ert-deftest clatter-test-notice-server-notice-routes-to-server-buffer ()
  "A NOTICE from a bare server name (no user/host) lands in the server buffer."
  (clatter-test-with-ui-connection conn
    (clatter-ui--on-notice conn '("irc.example" nil nil) "testnick" "server says hi" nil)
    (let ((server (clatter-get-server-buffer "testnet")))
      (should server)
      (should (eq 'server (buffer-local-value 'clatter--buffer-type server)))
      ;; No query buffer named after the server should be created.
      (should-not (clatter-get-buffer "testnet" "irc.example")))))

(ert-deftest clatter-test-notice-channel-notice-routes-to-channel ()
  "A channel NOTICE lands in the channel buffer, not the server buffer."
  (clatter-test-with-ui-connection conn
    (let ((chan (clatter-get-or-create-buffer "testnet" "#chan")))
      (clatter-ui-setup-buffer-if-needed chan)
      (clatter-ui--on-notice conn '("alice" "user" "host") "#chan" "chan notice" nil)
      (should (eq (clatter-get-buffer "testnet" "#chan") chan))
      (should-not (clatter-get-server-buffer "testnet")))))
;; --- Self echo ---

(ert-deftest clatter-test-optimistic-reply-renders-context ()
  "An optimistic reply immediately shows its parent context."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-reply" "me"))
                 (buf (clatter-get-or-create-buffer "echo-reply" "#test")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-insert-privmsg
               buf "alice" (propertize "parent" 'clatter-msgid "msg-1") conn)
              (clatter-ui--send-privmsg
               conn "#test" "answer" nil buf '(("+reply" . "msg-1")))
              (let ((button (next-button (point-min) t)))
                (should button)
                (should (equal (button-get button 'reply-to) "msg-1"))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-optimistic-self-echo-reconciles-server-metadata ()
  "An optimistic local line is replaced by its server echo metadata."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-one"))
                 (buf (clatter-get-or-create-buffer "echo-one" "#test")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "hello"))
            (should (= 1 (length (with-current-buffer buf clatter--pending-self-echoes))))
            (let ((server-text (propertize "hello" 'clatter-msgid "server-id"))
                  (server-time (encode-time 0 2 3 4 5 2026)))
              (clatter-ui--on-privmsg
               conn (clatter-parse-prefix "testnick!u@h") "#test" server-text server-time)
              (with-current-buffer buf
                (should-not clatter--pending-self-echoes)
                (let ((pos (clatter--find-message-position-by-msgid
                            buf "server-id")))
                  (should pos)
                  (should (equal (get-text-property pos 'clatter-server-time)
                                 server-time))
                  (should (equal (get-text-property pos 'clatter-text)
                                 server-text)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-optimistic-self-echo-queues-identical-messages ()
  "Identical optimistic sends consume one pending entry per server echo."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-two"))
                 (buf (clatter-get-or-create-buffer "echo-two" "#test"))
                 (sender (clatter-parse-prefix "testnick!u@h")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "same")
              (clatter-ui--send-privmsg conn "#test" "same"))
            (let ((before-first-echo (with-current-buffer buf (buffer-string))))
            (clatter-ui--on-privmsg conn sender "#test"
                                    (propertize "same" 'clatter-msgid "one") nil)
            (with-current-buffer buf
              (should (= 1 (length clatter--pending-self-echoes)))
              ;; Reconciliation updates the tentative line in place; a
              ;; duplicate server echo would change the buffer contents.
              (should (equal before-first-echo (buffer-string))))
            (clatter-ui--on-privmsg conn sender "#test"
                                    (propertize "same" 'clatter-msgid "two") nil)
            (with-current-buffer buf
              (should-not clatter--pending-self-echoes)
              (should (equal before-first-echo (buffer-string)))
              (should (clatter--find-message-position-by-msgid buf "one"))
              (should (clatter--find-message-position-by-msgid buf "two"))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-server-self-echo-waits-for-echo-message ()
  "The default mode retains the existing server-echo behavior."
  (let ((clatter-self-echo-mode 'server)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-three"))
                 (buf (clatter-get-or-create-buffer "echo-three" "#test")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "delayed")
              (should-not clatter--pending-self-echoes)
              (should-not (string-match-p "delayed" (buffer-string))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-optimistic-self-echo-without-echo-message-does-not-reconcile ()
  "Optimistic fallback does not retain state that swallows a later self message."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection-with-caps
                        '("server-time" "message-tags") "echo-no-cap"))
                 (buf (clatter-get-or-create-buffer "echo-no-cap" "#test"))
                 (sender (clatter-parse-prefix "testnick!u@h")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "fallback")
              (should-not clatter--pending-self-echoes))
            (clatter-ui--on-privmsg conn sender "#test"
                                    (propertize "fallback" 'clatter-msgid "late-server") nil)
            (with-current-buffer buf
              (should-not clatter--pending-self-echoes)
              (should (clatter--find-message-position-by-msgid buf "late-server"))
              (should (= 2 (how-many "fallback" (point-min) (point-max)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-expired-optimistic-self-echo-does-not-reconcile ()
  "A delayed self message is not reconciled with an expired local echo."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-self-echo-timeout 1)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-expired"))
                 (buf (clatter-get-or-create-buffer "echo-expired" "#test"))
                 (sender (clatter-parse-prefix "testnick!u@h")))
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "delayed")
              (setf (plist-get (car clatter--pending-self-echoes) :created-at) 0))
            (clatter-ui--on-privmsg conn sender "#test"
                                    (propertize "delayed" 'clatter-msgid "playback") nil)
            (with-current-buffer buf
              (should-not clatter--pending-self-echoes)
              (should (clatter--find-message-position-by-msgid buf "playback"))
              (should (= 2 (how-many "delayed" (point-min) (point-max)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-disconnect-clears-optimistic-self-echoes ()
  "Disconnecting clears pending local echoes before any later replay."
  (let ((clatter-self-echo-mode 'optimistic)
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "echo-disconnect"))
                 (buf (clatter-get-or-create-buffer "echo-disconnect" "#test"))
                 nonce)
            (clatter-ui-setup-buffer-if-needed buf)
            (with-current-buffer buf
              (clatter-ui--send-privmsg conn "#test" "before-disconnect")
              (setq nonce (plist-get (car clatter--pending-self-echoes) :nonce))
              (should clatter--pending-self-echoes))
            (clatter-ui--on-disconnect "echo-disconnect" "closed")
            (with-current-buffer buf
              (should-not clatter--pending-self-echoes)
              (should-not (text-property-any
                           (point-min) (point-max) 'clatter-self-echo-nonce nonce)))))
      (clatter-test-cleanup))))

;; --- Playback fool visibility ---

(defun clatter-test--property-at-text (text property)
  "Return PROPERTY at the first occurrence of TEXT in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward text nil t)
      (get-text-property (1- (point)) property))))

(ert-deftest clatter-test-playback-fools-are-toggleable ()
  "Fool messages in history playback get the fool invisibility category.
Live buffer messages tag fools with the `clatter-fool' invisible category
so /fools can toggle them; playback batches must do the same rather than
always showing fool messages."
  (let ((clatter-fools '("noisemaker"))
        (clatter-timestamp-side nil))
    (unwind-protect
        (clatter-test-with-mock-send
          (let* ((conn (clatter-test-make-connection "playback-fools"))
                 (buf (clatter-get-or-create-buffer "playback-fools" "#test")))
            (clatter-ui-setup-buffer-if-needed buf)
            (clatter-ui--on-batch-complete
             conn "chathistory" "#test"
             (list (list :type 'privmsg :sender "noisemaker"
                         :text "fool-playback-line" :time nil)
                   (list :type 'privmsg :sender "friend"
                         :text "pal-playback-line" :time nil)))
            (with-current-buffer buf
              (should (eq (clatter-test--property-at-text
                           "fool-playback-line" 'invisible)
                          'clatter-fool))
              (should-not (clatter-test--property-at-text
                           "pal-playback-line" 'invisible)))))
      (clatter-test-cleanup))))

;; --- Timestamp margins ---

(defun clatter-test--timestamp-overlay-count ()
  "Return the number of message timestamp overlays in the current buffer."
  (cl-count-if (lambda (overlay) (overlay-get overlay 'clatter-timestamp))
               (overlays-in (point-min) (point-max))))

(defun clatter-test--timestamp-overlay ()
  "Return the first message timestamp overlay in the current buffer."
  (cl-find-if (lambda (overlay) (overlay-get overlay 'clatter-timestamp))
              (overlays-in (point-min) (point-max))))

(ert-deftest clatter-test-timestamps-only-if-changed-coalesces-formatted-values ()
  "Repeated formatted timestamps use one margin timestamp when enabled."
  (let ((clatter-timestamp-only-if-changed t)
        (clatter-timestamp-format "%H:%M")
        (clatter-timestamp-side 'right)
        (first (encode-time 30 12 10 1 1 2026))
        (same-minute (encode-time 59 12 10 1 1 2026))
        (next-minute (encode-time 0 13 10 1 1 2026)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "first" nil nil first)
      (clatter--insert-message (current-buffer) "same minute" nil nil same-minute)
      (clatter--insert-message (current-buffer) "next minute" nil nil next-minute)
      (should (= (clatter-test--timestamp-overlay-count) 2)))))

(ert-deftest clatter-test-timestamps-only-if-changed-is-buffer-local ()
  "Timestamp suppression does not carry over to another buffer."
  (let ((clatter-timestamp-only-if-changed t)
        (clatter-timestamp-format "%H:%M")
        (time (encode-time 30 12 10 1 1 2026)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "first" nil nil time)
      (clatter--insert-message (current-buffer) "same buffer" nil nil time)
      (should (= (clatter-test--timestamp-overlay-count) 1)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "other buffer" nil nil time)
      (should (= (clatter-test--timestamp-overlay-count) 1)))))

(ert-deftest clatter-test-timestamps-only-if-changed-default-keeps-every-timestamp ()
  "The default preserves the current per-message timestamp behavior."
  (let ((clatter-timestamp-only-if-changed nil)
        (clatter-timestamp-format "%H:%M")
        (time (encode-time 30 12 10 1 1 2026)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "first" nil nil time)
      (clatter--insert-message (current-buffer) "second" nil nil time)
      (should (= (clatter-test--timestamp-overlay-count) 2)))))

(ert-deftest clatter-test-timestamp-side-left-margin ()
  "Left timestamp side configures the left margin."
  (let ((clatter-timestamp-side 'left)
        (clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (should (= left-margin-width 6))
      (should (= right-margin-width 0)))))

(ert-deftest clatter-test-timestamp-side-right-margin ()
  "Right timestamp side configures the right margin."
  (let ((clatter-timestamp-side 'right)
        (clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (should (= left-margin-width 0))
      (should (= right-margin-width 6)))))

(ert-deftest clatter-test-timestamp-side-sync-clears-stale-window-margin ()
  "Window margin sync clears the previous timestamp side."
  (let ((clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (switch-to-buffer (current-buffer))
      (let ((clatter-timestamp-side 'right))
        (clatter--sync-window-margins)
        (should-not (car (window-margins)))
        (should (= (cdr (window-margins)) 6)))
      (let ((clatter-timestamp-side 'left))
        (clatter--sync-window-margins)
        (should (= (car (window-margins)) 6))
        (should-not (cdr (window-margins))))
      (let ((clatter-timestamp-side nil))
        (clatter--sync-window-margins)
        (should-not (car (window-margins)))
        (should-not (cdr (window-margins))))
      (let ((clatter-timestamp-side 'divider))
        (clatter--sync-window-margins)
        (should-not (car (window-margins)))
        (should-not (cdr (window-margins))))
      (let ((clatter-timestamp-side 'inline))
        (clatter--sync-window-margins)
        (should-not (car (window-margins)))
        (should-not (cdr (window-margins)))))))

(ert-deftest clatter-test-timestamp-inline-aligns-to-right ()
  "Inline timestamps use an after-string aligned to the window edge."
  (let ((clatter-timestamp-side 'inline)
        (clatter-timestamp-format "%H:%M")
        (clatter-timestamp-only-if-changed nil)
        (conn (clatter-test-make-connection))
        (time (encode-time 0 12 10 1 1 2026)))
    (unwind-protect
        (with-temp-buffer
          (clatter-insert-privmsg (current-buffer) "alice" "hello" conn time)
          (let ((ov (clatter-test--timestamp-overlay)))
            (should ov)
            (should-not (overlay-get ov 'before-string))
            (let* ((after (overlay-get ov 'after-string))
                   (display (get-text-property 0 'display after)))
              (should (string-match-p "10:12" after))
              (should (eq (car display) 'space))
              (should (equal (plist-get (cdr display) :align-to) '(- right 5))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-timestamp-inline-break-when-wrapped ()
  "A line with no room left for the stamp gets no stamp at all."
  (let ((clatter-timestamp-side 'inline)
        (clatter-timestamp-format "%H:%M")
        (clatter-timestamp-tooltip-format "%H:%M:%S")
        (clatter-timestamp-only-if-changed nil)
        (clatter-fill-column nil)
        (conn (clatter-test-make-connection))
        (time (encode-time 0 12 10 1 1 2026))
        (msg (make-string (+ (frame-width) 50) ?x)))
    (unwind-protect
        (with-temp-buffer
          (clatter-insert-privmsg (current-buffer) "alice" msg conn time)
          (let ((ov (clatter-test--timestamp-overlay)))
            (should ov)
            ;; No stamp row of its own: a display-string row is anchored at a
            ;; position a neighbouring row already owns, so point cannot land
            ;; on it and vertical motion (evil j/k) gets trapped.
            (should-not (overlay-get ov 'before-string))
            (should-not (overlay-get ov 'after-string))
            (should (equal (overlay-get ov 'help-echo) "10:12:00"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-timestamp-margin-clears-inline-after-string ()
  "Reapplying a margin stamp drops a leftover inline after-string."
  (let ((clatter-timestamp-side 'inline)
        (clatter-timestamp-format "%H:%M")
        (clatter-timestamp-only-if-changed nil)
        (conn (clatter-test-make-connection))
        (time (encode-time 0 12 10 1 1 2026)))
    (unwind-protect
        (with-temp-buffer
          (clatter-insert-privmsg (current-buffer) "alice" "hello" conn time)
          (let ((ov (clatter-test--timestamp-overlay)))
            (should (overlay-get ov 'after-string))
            (let ((clatter-timestamp-side 'right))
              (clatter--timestamp-overlay-apply ov "10:12")
              (should-not (overlay-get ov 'after-string))
              (should (overlay-get ov 'before-string)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-timestamp-inline-stays-before-newline ()
  "Inline stamp overlay ends at the newline, not on the next message."
  (let ((clatter-timestamp-side 'inline)
        (clatter-timestamp-format "%H:%M")
        (clatter-timestamp-only-if-changed nil)
        (conn (clatter-test-make-connection))
        (t1 (encode-time 0 24 9 28 8 2026))
        (t2 (encode-time 0 29 9 28 8 2026)))
    (unwind-protect
        (with-temp-buffer
          (clatter-mode)
          (setq-local word-wrap t)
          (clatter-insert-privmsg (current-buffer) "alice" "hello" conn t1)
          (clatter-insert-privmsg (current-buffer) "bob" "there" conn t2)
          (let ((ovs (cl-remove-if-not
                      (lambda (overlay)
                        (overlay-get overlay 'clatter-timestamp))
                      (overlays-in (point-min) (point-max)))))
            (should (= (length ovs) 2))
            (dolist (ov ovs)
              (should-not (eq (char-after (overlay-start ov)) ?\n))
              (should (eq (char-after (overlay-end ov)) ?\n))
              (should (overlay-get ov 'after-string))
              (should-not (overlay-get ov 'before-string)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-timestamp-inline-stamps-system ()
  "Inline stamps the same lines margin modes stamp, system included."
  (let ((clatter-timestamp-side 'inline)
        (clatter-timestamp-only-if-changed nil)
        (clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-insert-system (current-buffer) "CTCP VERSION reply from knighthk")
      (should (= 1 (clatter-test--timestamp-overlay-count))))))

(ert-deftest clatter-test-timestamp-inline-drops-stamp-on-compact-growth ()
  "A compact append re-checks the stamp's fit; overflow drops it."
  (let ((clatter-timestamp-side 'inline)
        (clatter-timestamp-only-if-changed nil)
        (clatter-timestamp-format "%H:%M")
        (clatter-compact-system-messages 'compact)
        (clatter-compact-system-group-window 180)
        (times '(100.0 110.0 120.0))
        (long-nick (make-string (+ (frame-width) 20) ?x)))
    (clatter-test-with-ui-connection conn
      (ignore conn)
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (cl-letf (((symbol-function 'clatter--compact-system-now)
                   (lambda () (pop times))))
          (clatter--insert-system-event
           buffer 'join '(:nick "alice" :channel "#test") nil)
          (with-current-buffer buffer
            (should (overlay-get (clatter-test--timestamp-overlay)
                                 'after-string)))
          ;; A short append still fits: the stamp survives.
          (clatter--insert-system-event
           buffer 'join '(:nick "bob" :channel "#test") nil)
          (with-current-buffer buffer
            (should (overlay-get (clatter-test--timestamp-overlay)
                                 'after-string)))
          ;; Growing past the body width drops it, like a long message.
          (clatter--insert-system-event
           buffer 'join (list :nick long-nick :channel "#test") nil)
          (with-current-buffer buffer
            (should-not (overlay-get (clatter-test--timestamp-overlay)
                                     'after-string))))))))

(defun clatter-test--divider-positions ()
  "Return buffer positions of minute-divider lines."
  (clatter--navigation-property-positions 'clatter-timestamp-divider))

(ert-deftest clatter-test-timestamp-divider-no-margin ()
  "Divider timestamps do not reserve a window margin."
  (let ((clatter-timestamp-side 'divider)
        (clatter-timestamp-format "%H:%M"))
    (with-temp-buffer
      (clatter-mode)
      (should (= left-margin-width 0))
      (should (= right-margin-width 0)))))

(ert-deftest clatter-test-timestamp-divider-coalesced ()
  "Minute rows fire once per bucket, never twice in the same bucket."
  (let ((clatter-timestamp-side 'divider)
        (clatter-timestamp-format "%H:%M")
        (clatter-timestamp-only-if-changed nil)
        (conn (clatter-test-make-connection))
        (t1 (encode-time 30 12 10 1 1 2026))
        (t1b (encode-time 59 12 10 1 1 2026))
        (t2 (encode-time 0 13 10 1 1 2026)))
    (unwind-protect
        (with-temp-buffer
          (clatter-insert-privmsg (current-buffer) "alice" "hi" conn t1)
          (clatter-insert-privmsg (current-buffer) "alice" "again" conn t1b)
          (clatter-insert-privmsg (current-buffer) "bob" "yo" conn t2)
          (should (= 2 (length (clatter-test--divider-positions))))
          (should (string-match-p "— 10:12 —" (buffer-string)))
          (should (string-match-p "— 10:13 —" (buffer-string)))
          (let ((pos (clatter-test--divider-positions)))
            (should-not (eq (line-number-at-pos (nth 1 pos))
                            (1+ (line-number-at-pos (nth 0 pos)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-timestamp-margin-respects-interval ()
  "With only-if-changed, margin stamps coalesce to interval buckets too."
  (let ((clatter-timestamp-side 'right)
        (clatter-timestamp-only-if-changed t)
        (clatter-timestamp-interval 10)
        (clatter-timestamp-format "%H:%M")
        (conn (clatter-test-make-connection))
        (t1 (encode-time 0 12 10 1 1 2026))
        (t2 (encode-time 0 19 10 1 1 2026))
        (t3 (encode-time 0 22 10 1 1 2026)))
    (unwind-protect
        (with-temp-buffer
          (clatter-insert-privmsg (current-buffer) "alice" "a" conn t1)
          (clatter-insert-privmsg (current-buffer) "alice" "b" conn t2)
          (clatter-insert-privmsg (current-buffer) "bob" "c" conn t3)
          (should (= 2 (clatter-test--timestamp-overlay-count))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-timestamp-divider-rows-for-system-lines ()
  "System lines open rows like any stamped line, coalesced per bucket."
  (let ((clatter-timestamp-side 'divider)
        (clatter-timestamp-format "%H:%M")
        (t1 (encode-time 0 12 10 1 1 2026))
        (t2 (encode-time 10 12 10 1 1 2026)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "*** alice joined" nil nil t1 nil)
      (clatter--insert-message (current-buffer) "*** bob parted" nil nil t2 nil)
      (should (= 1 (length (clatter-test--divider-positions))))
      (should (string-match-p "— 10:12 —" (buffer-string))))))

(ert-deftest clatter-test-timestamp-divider-row-follows-opener-visibility ()
  "A row inherits its opening line's invisible categories."
  (let ((clatter-timestamp-side 'divider)
        (clatter-timestamp-format "%H:%M")
        (t1 (encode-time 0 12 10 1 1 2026)))
    (with-temp-buffer
      (clatter--insert-message (current-buffer) "*** alice joined" nil nil t1 'join)
      (let ((row (text-property-any (point-min) (point-max)
                                    'clatter-timestamp-divider t)))
        (should row)
        (should (eq 'join (get-text-property row 'invisible)))
        ;; Hidden with its opener under the default spec; shown once the
        ;; category is taken out.
        (should (invisible-p row))
        (let ((buffer-invisibility-spec '(other)))
          (should-not (invisible-p row)))))))

(ert-deftest clatter-test-timestamp-divider-respects-interval ()
  "Divider buckets span hour boundaries."
  (let ((clatter-timestamp-side 'divider)
        (clatter-timestamp-interval 90)
        (clatter-timestamp-format "%H:%M")
        (conn (clatter-test-make-connection))
        (t1 (encode-time 0 50 10 1 1 2026))
        (t2 (encode-time 0 10 11 1 1 2026))
        (t3 (encode-time 0 20 12 1 1 2026)))
    (unwind-protect
        (with-temp-buffer
          (clatter-insert-privmsg (current-buffer) "alice" "a" conn t1)
          (clatter-insert-privmsg (current-buffer) "alice" "b" conn t2)
          (clatter-insert-privmsg (current-buffer) "bob" "c" conn t3)
          (should (= 2 (length (clatter-test--divider-positions))))
          (should (string-match-p "— 10:50 —" (buffer-string)))
          (should-not (string-match-p "— 11:10 —" (buffer-string)))
          (should (string-match-p "— 12:20 —" (buffer-string))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-timestamp-divider-seeds-on-setup ()
  "A new buffer opens with a divider before its first message."
  (let ((clatter-timestamp-side 'divider)
        (clatter-timestamp-interval 10)
        (clatter-timestamp-format "%H:%M")
        (conn (clatter-test-make-connection))
        (now (encode-time 0 12 10 1 1 2026))
        (later (encode-time 0 22 10 1 1 2026)))
    (unwind-protect
        (dolist (clatter-message-order '(oldest-first newest-first))
          (with-temp-buffer
            (clatter-mode)
            (setq-local clatter--network "testnet")
            (setq-local clatter--target "#test")
            (cl-letf (((symbol-function 'current-time) (lambda () now)))
              (clatter-ui-setup-buffer (current-buffer)))
            (should (= 1 (length (clatter-test--divider-positions))))
            (should (string-match-p "— 10:12 —" (buffer-string)))
            ;; Same bucket: reuse the seed and keep it before the message.
            (clatter-insert-privmsg (current-buffer) "alice" "hi" conn now)
            (should (= 1 (length (clatter-test--divider-positions))))
            (should (< (car (clatter-test--divider-positions))
                       (string-match-p "hi" (buffer-string))))
            ;; A later bucket still opens a new row.
            (clatter-insert-privmsg (current-buffer) "bob" "yo" conn later)
            (should (= 2 (length (clatter-test--divider-positions))))))
      (clatter-test-cleanup))))

;; --- Message filling ---

(ert-deftest clatter-test-insert-message-fills-at-fill-column ()
  "Inserted messages are hard-wrapped at `clatter-fill-column'."
  (let ((clatter-fill-column 40)
        (clatter-nick-column-width 7))
    (with-temp-buffer
      (clatter--insert-message
       (current-buffer)
       "<alice> this is a long message that should wrap around the configured fill column"
       t)
      (should
       (equal (buffer-string)
              "<alice> this is a long message that\n        should wrap around the\n        configured fill column\n")))))

(ert-deftest clatter-test-effective-fill-column ()
  "`clatter--effective-fill-column' resolves integer, auto and nil."
  (let ((clatter-nick-column-width 7))
    (with-temp-buffer
      ;; An integer above the nick-indent floor passes through.
      (let ((clatter-fill-column 40))
        (should (= 40 (clatter--effective-fill-column))))
      ;; An integer at/below the floor disables wrapping (returns nil).
      (let ((clatter-fill-column 8))           ; floor = 1+7 = 8
        (should (null (clatter--effective-fill-column))))
      ;; nil disables wrapping.
      (let ((clatter-fill-column nil))
        (should (null (clatter--effective-fill-column))))
      ;; `auto' with no live window falls back to the frame width; pin it so
      ;; the result is deterministic.
      (let ((clatter-fill-column 'auto)
            (clatter-max-line-length 400))
        (cl-letf (((symbol-function 'frame-width) (lambda (&optional _) 30)))
          (should (= 30 (clatter--effective-fill-column))))
        ;; `auto' is capped at `clatter-max-line-length'.
        (let ((clatter-max-line-length 20))
          (cl-letf (((symbol-function 'frame-width) (lambda (&optional _) 200)))
            (should (= 20 (clatter--effective-fill-column)))))))))

(ert-deftest clatter-test-insert-message-fills-at-auto-column ()
  "With `clatter-fill-column' `auto', messages hard-wrap to the derived column."
  (let ((clatter-fill-column 'auto)
        (clatter-nick-column-width 7)
        (clatter-max-line-length 400))
    (with-temp-buffer
      (cl-letf (((symbol-function 'frame-width) (lambda (&optional _) 30)))
        (clatter--insert-message
         (current-buffer)
         "<alice> this is a long message that should wrap around the auto column"
         t))
      ;; A wrapped message produces more than the single trailing newline.
      (should (> (cl-count ?\n (buffer-string)) 1)))))

;; --- Fool visibility ---

;; --- Smart noise visibility ---

(ert-deftest clatter-test-smart-noise-seeds-new-buffer-by-default ()
  "New clatter buffers hide smart-filtered noise by default."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join)))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should (memq 'noise buffer-invisibility-spec)))))

(ert-deftest clatter-test-smart-noise-disabled-does-not-seed-new-buffer ()
  "Disabling smart noise leaves the automatic category out of new buffers."
  (let ((clatter-smart-enabled nil)
        (clatter-smart-noise '(join))
        (clatter-suppress-messages '(muted)))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should-not (memq 'noise buffer-invisibility-spec)))))

(ert-deftest clatter-test-empty-smart-noise-does-not-seed-new-buffer ()
  "No automatic category is added when no message types are smart-filtered."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise nil)
        (clatter-suppress-messages '(muted)))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should-not (memq 'noise buffer-invisibility-spec)))))

(ert-deftest clatter-test-explicit-noise-suppression-is-preserved ()
  "Global noise suppression remains effective when smart noise is disabled."
  (let ((clatter-smart-enabled nil)
        (clatter-smart-noise nil)
        (clatter-suppress-messages '(muted noise)))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should (memq 'noise buffer-invisibility-spec)))))

;; --- Compact system messages ---

(defun clatter-test--visible-text (start end)
  "Return visually displayed text between START and END.
Honor the text invisibility and string `display' properties used by compact
system messages."
  (let ((position start)
        pieces)
    (while (< position end)
      (cond
       ((invisible-p position)
        (setq position
              (or (next-single-property-change
                   position 'invisible nil end)
                  end)))
       ((stringp (get-text-property position 'display))
        (push (get-text-property position 'display) pieces)
        (setq position
              (or (next-single-property-change position 'display nil end)
                  end)))
       (t
        (push (buffer-substring-no-properties position (1+ position)) pieces)
        (cl-incf position))))
    (apply #'concat (nreverse pieces))))

(defun clatter-test--compact-group-bounds ()
  "Return bounds of the first compact system group in the current buffer."
  (when-let* ((start (text-property-not-all
                      (point-min) (point-max)
                      'clatter-compact-system-group-id nil)))
    (cons start
          (or (next-single-property-change
               start 'clatter-compact-system-group-id nil (point-max))
              (point-max)))))

(ert-deftest clatter-test-compact-system-event-presets ()
  "Compact presets format every supported event with the promised context."
  (dolist
      (case
       '((join (:nick "alice" :channel "#test" :realname "Alice")
               "alice" "alice" "alice (Alice) #test")
         (part (:nick "alice" :channel "#test" :reason "bye")
               "alice" "alice — bye" "alice #test — bye")
         (quit (:nick "alice" :channel "#test" :reason "timeout")
               "alice" "alice — timeout" "alice #test — timeout")
         (nick (:nick "alice" :new-nick "alice_")
               "alice → alice_" "alice → alice_" "alice → alice_")
         (away (:nick "alice" :channel "#test" :reason "lunch")
               "alice" "alice — lunch" "alice #test — lunch")
         (back (:nick "alice" :channel "#test")
               "alice" "alice" "alice #test")
         (mode (:nick "alice" :channel "#test" :modes "+o bob")
               "alice +o bob" "alice +o bob" "alice #test +o bob")
         (kick (:nick "bob" :setter "alice" :channel "#test" :reason "flooding")
               "bob ← alice" "bob ← alice — flooding"
               "bob ← alice #test — flooding")
         (invite (:nick "alice" :invitee "testnick" :channel "#secret")
                 "alice → #secret" "alice → #secret"
                 "alice → testnick #secret")))
    (pcase-let ((`(,event ,fields ,essential ,reasons ,full) case))
      (let ((clatter-compact-system-messages 'essential))
        (should (equal (clatter--format-system-event event fields) essential)))
      (let ((clatter-compact-system-messages 'reasons))
        (should (equal (clatter--format-system-event event fields) reasons)))
      (let ((clatter-compact-system-messages 'full))
        (should (equal (clatter--format-system-event event fields) full))))))

(ert-deftest clatter-test-compact-system-groups-consecutive-events ()
  "Compact mode groups adjacent presence actions in either message order."
  (dolist (order '(oldest-first newest-first))
    (let ((clatter-compact-system-messages 'compact)
          (clatter-compact-system-group-window 180)
          (clatter-message-order order)
          (times '(100.0 110.0 120.0)))
      (clatter-test-with-ui-connection conn
        (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
          (cl-letf (((symbol-function 'clatter--compact-system-now)
                     (lambda () (pop times))))
            (clatter--insert-system-event
             buffer 'quit '(:nick "alcor" :channel "#test") 'quit)
            (clatter--insert-system-event
             buffer 'join '(:nick "alcor" :channel "#test") 'join)
            (clatter--insert-system-event
             buffer 'away '(:nick "Elouin" :channel "#test") 'away))
          (with-current-buffer buffer
            (let ((rendered (buffer-substring-no-properties
                             (point-min) (point-max))))
              (should (string-match-p
                       "× alcor · → alcor · ○ Elouin" rendered)))
            (goto-char (point-min))
            (search-forward "×")
            (should (memq 'quit (ensure-list
                                 (get-text-property (1- (point)) 'invisible))))
            (search-forward "→")
            (should (memq 'join (ensure-list
                                 (get-text-property (1- (point)) 'invisible))))
            (search-forward "○")
            (should (memq 'away (ensure-list
                                 (get-text-property (1- (point)) 'invisible))))))))))

(ert-deftest clatter-test-compact-system-layout-visibility-matrix ()
  "Every hidden-action combination preserves compact alignment and spacing."
  (dolist (order '(oldest-first newest-first))
    (dolist (hidden '(()
                      (quit) (part) (join) (away)
                      (quit part) (quit join) (quit away)
                      (part join) (part away) (join away)
                      (quit part join) (part join away)
                      (quit part join away)))
      (let ((clatter-compact-system-messages 'compact)
            (clatter-message-order order)
            (clatter-nick-column-width 14)
            (clatter-smart-enabled nil)
            (clatter-suppress-messages (append hidden '(muted))))
        (clatter-test-with-ui-connection conn
          (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
            (clatter-ui-setup-buffer buffer)
            (clatter--insert-system-event
             buffer 'quit '(:nick "alice" :channel "#test") 'quit)
            (clatter--insert-system-event
             buffer 'part '(:nick "bob" :channel "#test") 'part)
            (clatter--insert-system-event
             buffer 'join '(:nick "carol" :channel "#test") 'join)
            (clatter--insert-system-event
             buffer 'away '(:nick "dave" :channel "#test") 'away)
            (with-current-buffer buffer
              (pcase-let* ((`(,start . ,end)
                            (clatter-test--compact-group-bounds))
                           (visible-events
                            (delq nil
                                  (list (unless (memq 'quit hidden) "× alice")
                                        (unless (memq 'part hidden) "← bob")
                                        (unless (memq 'join hidden) "→ carol")
                                        (unless (memq 'away hidden) "○ dave"))))
                           (expected
                            (if visible-events
                                (concat (make-string 13 ?\s)
                                        (string-join visible-events " · ")
                                        "\n")
                              "")))
                (should (equal (clatter-test--visible-text start end)
                               expected))
                (dotimes (_ 2)
                  (visible-mode 1)
                  (should
                   (equal (clatter-test--visible-text start end)
                          (concat (make-string 13 ?\s)
                                  "× alice · ← bob · → carol · ○ dave\n")))
                  (visible-mode -1)
                  (should (equal (clatter-test--visible-text start end)
                                 expected)))))))))))

(ert-deftest clatter-test-compact-system-mixes-smart-visibility-in-one-group ()
  "Noise-tagged actions group with visible actions without misalignment."
  (dolist (order '(oldest-first newest-first))
    (let ((clatter-compact-system-messages 'compact)
          (clatter-message-order order)
          (clatter-nick-column-width 14)
          (clatter-smart-enabled t)
          (clatter-smart-noise '(join part quit away))
          (clatter-suppress-messages '(noise muted)))
      (clatter-test-with-ui-connection conn
        (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
          (clatter-ui-setup-buffer buffer)
          (clatter--insert-system-event
           buffer 'quit '(:nick "alice" :channel "#test") '(quit noise))
          (clatter--insert-system-event
           buffer 'part '(:nick "bob" :channel "#test") 'part)
          (clatter--insert-system-event
           buffer 'join '(:nick "carol" :channel "#test") '(join noise))
          (clatter--insert-system-event
           buffer 'away '(:nick "dave" :channel "#test") '(away noise))
          (with-current-buffer buffer
            (let ((groups 0)
                  (position (point-min)))
              (while-let ((start
                           (text-property-not-all
                            position (point-max)
                            'clatter-compact-system-group-id nil)))
                (cl-incf groups)
                (setq position
                      (or (next-single-property-change
                           start 'clatter-compact-system-group-id
                           nil (point-max))
                          (point-max))))
              (should (= groups 1)))
            (pcase-let ((`(,start . ,end)
                          (clatter-test--compact-group-bounds)))
              (should (equal (clatter-test--visible-text start end)
                             (concat (make-string 13 ?\s) "← bob\n")))
              (visible-mode 1)
              (should
               (equal (clatter-test--visible-text start end)
                      (concat (make-string 13 ?\s)
                              "× alice · ← bob · → carol · ○ dave\n")))
              (visible-mode -1)
              (should (equal (clatter-test--visible-text start end)
                             (concat (make-string 13 ?\s) "← bob\n"))))))))))

(ert-deftest clatter-test-compact-system-appends-preserve-visual-line-prefixes ()
  "Appended compact actions retain wrapping alignment in Visual Line mode."
  (let ((clatter-compact-system-messages 'compact)
        (clatter-message-order 'oldest-first)
        (clatter-nick-column-width 14))
    (clatter-test-with-ui-connection conn
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (clatter-ui-setup-buffer buffer)
        (with-current-buffer buffer
          (visual-line-mode 1))
        (clatter--insert-system-event
         buffer 'join '(:nick "alice" :channel "#test") 'join)
        (clatter--insert-system-event
         buffer 'part '(:nick "bob" :channel "#test") 'part)
        (with-current-buffer buffer
          (goto-char (point-min))
          (search-forward "← bob")
          (let ((position (match-beginning 0)))
            (should (equal (get-text-property position 'wrap-prefix)
                           (make-string 15 ?\s)))
            (should (equal (get-text-property position 'line-prefix) ""))
            ;; Layout refresh must also repair compact segments left behind
            ;; by an older loaded implementation.
            (let ((inhibit-read-only t))
              (remove-text-properties
               position (+ position (length "← bob"))
               '(wrap-prefix nil line-prefix nil)))
            (should-not (get-text-property position 'wrap-prefix))
            (clatter--refresh-compact-system-layout)
            (should (equal (get-text-property position 'wrap-prefix)
                           (make-string 15 ?\s)))
            (should (equal (get-text-property position 'line-prefix) ""))))))))

(ert-deftest clatter-test-compact-system-layout-does-not-record-undo ()
  "Presentation refreshes do not grow the user's input undo history."
  (let ((clatter-compact-system-messages 'compact))
    (clatter-test-with-ui-connection conn
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (clatter-ui-setup-buffer buffer)
        (clatter--insert-system-event
         buffer 'join '(:nick "alice" :channel "#test") 'join)
        (with-current-buffer buffer
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (clatter--refresh-compact-system-layout)
          (should-not buffer-undo-list))))))

(ert-deftest clatter-test-compact-system-visible-later-event-keeps-indent ()
  "A visible later action retains indentation when the first action is hidden."
  (let ((clatter-compact-system-messages 'compact)
        (clatter-message-order 'oldest-first)
        (clatter-nick-column-width 14)
        (clatter-smart-enabled nil)
        (clatter-suppress-messages '(noise muted)))
    (clatter-test-with-ui-connection conn
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (clatter-ui-setup-buffer buffer)
        (clatter--insert-system-event
         buffer 'join '(:nick "hidden-user" :channel "#test")
         '(join noise))
        (clatter--insert-system-event
         buffer 'part '(:nick "visible-user" :channel "#test")
         '(part noise))
        (with-current-buffer buffer
          ;; Reproduce a mixed group whose later event has been reclassified
          ;; as visible while its original prefix remains noise-suppressed.
          (goto-char (point-min))
          (search-forward "← visible-user")
          (let ((event-start (match-beginning 0))
                (event-end (match-end 0)))
            (let ((inhibit-read-only t))
              (put-text-property event-start event-end 'invisible 'part)))
          (clatter--refresh-compact-system-layout)
          (pcase-let ((`(,start . ,end)
                       (clatter-test--compact-group-bounds)))
            (should-not (invisible-p start))
            (should
             (equal (clatter-test--visible-text start end)
                    (concat (make-string 13 ?\s)
                            "← visible-user\n")))))))))

(ert-deftest clatter-test-compact-system-separator-is-configurable ()
  "Compact grouped events use the configured separator."
  (let ((clatter-compact-system-messages 'compact)
        (clatter-compact-system-separator ", "))
    (clatter-test-with-ui-connection conn
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (clatter--insert-system-event
         buffer 'join '(:nick "alice" :channel "#test") 'join)
        (clatter--insert-system-event
         buffer 'join '(:nick "bob" :channel "#test") 'join)
        (with-current-buffer buffer
          (should (string-match-p
                   "→ alice, → bob"
                   (buffer-substring-no-properties
                    (point-min) (point-max)))))))))

(ert-deftest clatter-test-compact-system-clear-cannot-append-to-input ()
  "Clearing a compact group prevents later events entering the prompt."
  (dolist (order '(oldest-first newest-first))
    (let ((clatter-compact-system-messages 'compact)
          (clatter-compact-system-separator " · ")
          (clatter-message-order order)
          (clatter-prompt-format "[trev]: "))
      (clatter-test-with-ui-connection conn
        (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
          (clatter-ui-setup-buffer buffer)
          (clatter--insert-system-event
           buffer 'join '(:nick "old" :channel "#test") 'join)
          (with-current-buffer buffer
            (clatter-cmd-clear nil)
            (should-not clatter--compact-system-group))
          (clatter--insert-system-event
           buffer 'back '(:nick "Elouin" :channel "#test") 'away)
          (clatter--insert-system-event
           buffer 'join '(:nick "in0rdr" :channel "#test") 'join)
          (with-current-buffer buffer
            (let ((rendered (buffer-substring-no-properties
                             (point-min) (point-max))))
              (should (string-match-p
                       "● Elouin · → in0rdr\n" rendered))
              (should-not (string-match-p
                           "in0rdr \\[trev\\]:" rendered)))))))))

(ert-deftest clatter-test-compact-system-rejects-stale-tail-marker ()
  "A deleted compact line cannot leave an append anchor at the prompt."
  (let ((clatter-compact-system-messages 'compact)
        (clatter-message-order 'oldest-first)
        (clatter-prompt-format "[trev]: "))
    (clatter-test-with-ui-connection conn
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (clatter-ui-setup-buffer buffer)
        (clatter--insert-system-event
         buffer 'join '(:nick "old" :channel "#test") 'join)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            ;; Simulate an external history deletion that does not know about
            ;; compact grouping state.
            (delete-region (point-min) clatter--prompt-marker)))
        (clatter--insert-system-event
         buffer 'join '(:nick "new" :channel "#test") 'join)
        (with-current-buffer buffer
          (should (string-match-p
                   "→ new\n *\\[trev\\]:"
                   (buffer-substring-no-properties
                    (point-min) (point-max)))))))))

(ert-deftest clatter-test-compact-system-intervening-message-ends-group ()
  "Any intervening line prevents a later event joining an older group."
  (let ((clatter-compact-system-messages 'compact)
        (clatter-compact-system-group-window 180))
    (clatter-test-with-ui-connection conn
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (clatter--insert-system-event
         buffer 'join '(:nick "alice" :channel "#test") 'join)
        (clatter-insert-system buffer "intervening chat")
        (clatter--insert-system-event
         buffer 'join '(:nick "bob" :channel "#test") 'join)
        (with-current-buffer buffer
          (should (= (how-many "→" (point-min) (point-max)) 2))
          (goto-char (point-min))
          (should-not (search-forward "→ alice · → bob" nil t)))))))

(ert-deftest clatter-test-compact-system-respects-window-and-visibility ()
  "Expired or differently hidden events start separate compact groups."
  (let ((clatter-compact-system-messages 'compact)
        (clatter-compact-system-group-window 180)
        (times '(100.0 400.0 410.0 420.0 430.0)))
    (clatter-test-with-ui-connection conn
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (cl-letf (((symbol-function 'clatter--compact-system-now)
                   (lambda () (pop times))))
          (clatter--insert-system-event
           buffer 'join '(:nick "alice" :channel "#test") 'join)
          (clatter--insert-system-event
           buffer 'join '(:nick "bob" :channel "#test") 'join)
          (clatter--insert-system-event
           buffer 'join '(:nick "lurker" :channel "#test") '(join noise)))
        (with-current-buffer buffer
          (should (= (cl-count ?→ (buffer-substring-no-properties
                                   (point-min) (point-max)))
                     3)))))))

(ert-deftest clatter-test-compact-system-visible-mode-restores-layout ()
  "Disabling Visible mode keeps partially suppressed groups separated."
  (let ((clatter-compact-system-messages 'compact)
        (clatter-message-order 'oldest-first)
        (clatter-suppress-messages '(join muted)))
    (clatter-test-with-ui-connection conn
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (clatter-ui-setup-buffer buffer)
        (clatter--insert-system-event
         buffer 'quit '(:nick "alcor" :channel "#test") 'quit)
        (clatter--insert-system-event
         buffer 'join '(:nick "hidden" :channel "#test") 'join)
        (clatter--insert-system-event
         buffer 'away '(:nick "Elouin" :channel "#test") 'away)
        (with-current-buffer buffer
          (visible-mode 1)
          (visible-mode -1)
          (goto-char (point-min))
          (let (separators)
            (while (search-forward clatter-compact-system-separator nil t)
              (push (match-beginning 0) separators))
            (setq separators (nreverse separators))
            (should (= (length separators) 2))
            (should (equal (get-text-property (car separators) 'display) ""))
            (should-not (get-text-property (cadr separators) 'display))
            (should-not (invisible-p (cadr separators))))
          ;; The indentation belongs to the group rather than its first event.
          (goto-char (point-min))
          (should-not (invisible-p (point))))))))

(ert-deftest clatter-test-compact-system-visible-action-keeps-prompt-boundary ()
  "A partly hidden compact line cannot visually merge with the prompt."
  (let ((clatter-compact-system-messages 'compact)
        (clatter-message-order 'oldest-first)
        (clatter-prompt-format "[testnick]: ")
        (clatter-suppress-messages '(quit away muted)))
    (clatter-test-with-ui-connection conn
      (let ((buffer (clatter-get-or-create-buffer "testnet" "#test")))
        (clatter-ui-setup-buffer buffer)
        (clatter--insert-system-event
         buffer 'quit '(:nick "alice" :channel "#test") 'quit)
        (clatter--insert-system-event
         buffer 'join '(:nick "alice" :channel "#test") 'join)
        (clatter--insert-system-event
         buffer 'away '(:nick "bob" :channel "#test") 'away)
        (with-current-buffer buffer
          (let ((newline (1- (marker-position clatter--prompt-marker))))
            ;; Reproduce the stale mixed-visibility state reported from a
            ;; long-running buffer, then require layout refresh to repair it.
            (let ((inhibit-read-only t))
              (put-text-property newline (1+ newline) 'invisible 'noise))
            (clatter--refresh-compact-system-layout)
            (should (eq (char-after newline) ?\n))
            (should-not (invisible-p newline))
            (should (= (1+ newline)
                       (marker-position clatter--prompt-marker)))))))))

(ert-deftest clatter-test-compact-system-default-preserves-verbose-join ()
  "The default nil compact setting preserves the existing JOIN sentence."
  (let ((clatter-compact-system-messages nil)
        (clatter-smart-enabled nil)
        (clatter-suppress-messages '(muted)))
    (clatter-test-with-ui-connection conn
      (clatter-ui--on-join conn '("alice" nil nil) "#test" nil "Alice")
      (with-current-buffer (clatter-get-buffer "testnet" "#test")
        (goto-char (point-min))
        (should (search-forward "*** alice (Alice) has joined #test" nil t))))))

(ert-deftest clatter-test-compact-system-custom-symbol-preserves-noise ()
  "Compact JOINs use configured prefixes without losing noise metadata."
  (let ((clatter-compact-system-messages 'essential)
        (clatter-compact-system-symbols '((join . "+")))
        (clatter-smart-enabled t)
        (clatter-smart-noise '(join))
        (clatter-suppress-messages '(muted)))
    (clatter-test-with-ui-connection conn
      (cl-letf (((symbol-function 'clatter-smart-eval)
                 (lambda (&rest _args) t)))
        (clatter-ui--on-join conn '("lurker" nil nil) "#test" nil nil))
      (with-current-buffer (clatter-get-buffer "testnet" "#test")
        (goto-char (point-min))
        (should (search-forward "+ lurker" nil t))
        (let ((start (match-beginning 0)))
          (should (eq (get-text-property start 'face) 'clatter-system))
          (should (memq 'join
                        (ensure-list (get-text-property start 'invisible))))
          (should (memq 'noise
                        (ensure-list (get-text-property start 'invisible)))))))))

(ert-deftest clatter-test-compact-system-handlers-use-event-renderer ()
  "Every supported handler emits its compact event form."
  (let ((clatter-compact-system-messages 'essential)
        (clatter-smart-enabled nil)
        (clatter-suppress-messages '(muted)))
    (clatter-test-with-ui-connection conn
      (clatter-ui--on-join conn '("alice" nil nil) "#test" nil nil)
      (clatter-ui--on-mode conn "#test" '("op" nil nil) '("+o" "alice"))
      (clatter-ui--on-away conn '("alice" nil nil) "lunch")
      (clatter-ui--on-away conn '("alice" nil nil) nil)
      (clatter-ui--on-nick conn '("alice" nil nil) "alice_")
      (clatter-ui--on-join conn '("bob" nil nil) "#test" nil nil)
      (clatter-ui--on-part conn '("bob" nil nil) "#test" "bye")
      (clatter-ui--on-join conn '("carol" nil nil) "#test" nil nil)
      (clatter-ui--on-quit conn '("carol" nil nil) "timeout")
      (clatter-ui--on-join conn '("dave" nil nil) "#test" nil nil)
      (clatter-ui--on-kick conn "#test" '("op" nil nil) "dave" "flooding")
      (clatter-ui--on-invite conn '("op" nil nil) "testnick" "#test")
      (with-current-buffer (clatter-get-buffer "testnet" "#test")
        (let ((rendered (buffer-substring-no-properties (point-min) (point-max))))
          (dolist (expected '("→ alice" "± op +o alice" "○ alice" "● alice"
                              "» alice → alice_" "← bob" "× carol"
                              "⬾ dave ← op" "✉ op → #test"))
            (should (string-match-p (regexp-quote expected) rendered))))))))

(ert-deftest clatter-test-smart-noise-tags-noisy-events-in-new-buffer ()
  "A smart-filtered event in a new buffer carries the hidden noise category."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join))
        (clatter--buffer-alist nil)
        (conn (clatter-test-make-connection)))
    (unwind-protect
          (cl-letf (((symbol-function 'clatter-smart-eval)
                   (lambda (&rest _args) t)))
          (clatter-ui--on-join conn '("noisy" nil nil) "#test" nil nil)
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (should buffer)
            (with-current-buffer buffer
              (should (memq 'noise buffer-invisibility-spec))
              (goto-char (point-min))
              (search-forward "noisy has joined")
              (should (memq 'noise
                            (ensure-list (get-text-property (match-beginning 0) 'invisible)))))))
      (dolist (buffer (clatter-all-buffers))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-smart-noise-hides-first-join-from-non-chatter ()
  "A nick with no prior channel signal has its first smart-noise event hidden."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join part away))
        (clatter-suppress-messages '(muted))
        (clatter--buffer-alist nil)
        (conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (clatter-ui--on-join conn '("lurker" nil nil) "#test" nil nil)
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (should buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "lurker has joined")
              (should (memq 'noise
                            (ensure-list (get-text-property (match-beginning 0) 'invisible)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-smart-noise-keeps-part-from-active-nick ()
  "A nick that chatted in a channel keeps its later PART visible."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join part away))
        (clatter-suppress-messages '(muted))
        (clatter--buffer-alist nil)
        (conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (clatter-ui--on-privmsg conn '("alice" nil nil) "#test" "hello" nil)
          (clatter-ui--on-part conn '("alice" nil nil) "#test" "bye")
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (should buffer)
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "alice has left #test")
              (should-not (memq 'noise
                                (ensure-list (get-text-property (match-beginning 0) 'invisible)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-smart-noise-keeps-away-from-active-nick ()
  "A nick that chatted in a channel keeps its later AWAY visible."
  (let ((clatter-smart-enabled t)
        (clatter-smart-noise '(join part away))
        (clatter-suppress-messages '(muted))
        (clatter--buffer-alist nil)
        (conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (clatter-ui--on-privmsg conn '("alice" nil nil) "#test" "hello" nil)
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (should buffer)
            (with-current-buffer buffer
              (clatter-nick-add buffer "alice")))
          (clatter-ui--on-away conn '("alice" nil nil) "away")
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "alice is away: away")
              (should-not (memq 'noise
                                (ensure-list (get-text-property (match-beginning 0) 'invisible)))))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-fool-image-links-are-not-previewed ()
  "Inline image scanning skips fool messages."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-fools '("fool"))
        scanned)
    (unwind-protect
        (with-temp-buffer
          (clatter-mode)
          (setq-local clatter--network "testnet")
          (setq-local clatter--target "#test")
          (clatter-ui-setup-buffer (current-buffer))
          (cl-letf (((symbol-function 'clatter-image--scan-message)
                     (lambda (&rest _) (setq scanned t))))
            (clatter-insert-privmsg
             (current-buffer) "fool" "https://example.com/no.png" conn))
          (should-not scanned))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-test-fool-message-gets-dim-face ()
  "Messages with the fool invisibility category get `clatter-fool'."
  (with-temp-buffer
    (clatter--insert-message (current-buffer) "<fool> no thanks" t nil nil 'clatter-fool)
    (should (eq (get-text-property (point-min) 'invisible) 'clatter-fool))
    (should (memq 'clatter-fool (ensure-list (get-text-property (point-min) 'face))))))

(ert-deftest clatter-test-fool-face-takes-priority ()
  "The `clatter-fool' face is prepended to existing faces."
  (with-temp-buffer
    (clatter--insert-message
     (current-buffer)
     (propertize "<fool> no thanks" 'face 'clatter-notice)
     t nil nil 'clatter-fool)
    (should (equal (ensure-list (get-text-property (point-min) 'face))
                   '(clatter-fool clatter-notice)))))

(ert-deftest clatter-test-fool-presence-events-are-omitted ()
  "Fool JOIN, PART, and QUIT events are never inserted into chat buffers."
  (dolist (style '(nil compact))
    (dolist (visible '(nil t))
      (let ((clatter-compact-system-messages style)
            (clatter-fools '("fool"))
            (clatter-fools-visible visible)
            (clatter-smart-enabled nil))
        (clatter-test-with-ui-connection conn
          (clatter-ui--on-join conn '("fool" nil nil) "#test" nil nil)
          (let ((buffer (clatter-get-buffer "testnet" "#test")))
            (should (gethash "fool"
                             (buffer-local-value 'clatter--nick-list buffer)))
            (clatter-ui--on-part conn '("fool" nil nil) "#test" "bye")
            (should-not (gethash "fool"
                                 (buffer-local-value 'clatter--nick-list buffer)))
            (clatter-ui--on-join conn '("fool" nil nil) "#test" nil nil)
            (clatter-ui--on-quit conn '("fool" nil nil) "gone")
            (should-not (gethash "fool"
                                 (buffer-local-value 'clatter--nick-list buffer)))
            (with-current-buffer buffer
              (should-not (string-match-p
                           "fool"
                           (buffer-substring-no-properties
                            (point-min) (point-max)))))))))))

(ert-deftest clatter-test-fool-visibility-seeds-buffer-invisibility ()
  "New clatter buffers hide fools unless `clatter-fools-visible' is non-nil."
  (let ((clatter-fools-visible nil))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should (memq 'clatter-fool buffer-invisibility-spec))))
  (let ((clatter-fools-visible t))
    (with-temp-buffer
      (clatter-mode)
      (clatter-ui-setup-buffer (current-buffer))
      (should-not (memq 'clatter-fool buffer-invisibility-spec)))))

(ert-deftest clatter-test-toggle-fools-updates-existing-buffer ()
  "Toggling fool visibility updates existing clatter buffers."
  (let ((old clatter-fools-visible))
    (unwind-protect
        (with-temp-buffer
          (clatter-mode)
          (setq buffer-invisibility-spec '(clatter-fool muted))
          (clatter-toggle-fools 1)
          (should clatter-fools-visible)
          (should-not (memq 'clatter-fool buffer-invisibility-spec))
          (clatter-toggle-fools -1)
          (should-not clatter-fools-visible)
          (should (memq 'clatter-fool buffer-invisibility-spec)))
      (setq clatter-fools-visible old))))

(ert-deftest clatter-test-suppress-preserves-fool-visibility ()
  "Generic suppression commands keep fool visibility independent."
  (let ((clatter-fools-visible nil))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(clatter-fool muted))
      (clatter-cmd-suppress "none")
      (should (equal buffer-invisibility-spec '(clatter-fool)))
      (clatter-cmd-suppress "all")
      (should (memq 'clatter-fool buffer-invisibility-spec))))
  (let ((clatter-fools-visible t))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(clatter-fool muted))
      (clatter-cmd-suppress "none")
      (should-not (memq 'clatter-fool buffer-invisibility-spec))
      (clatter-cmd-suppress "all")
      (should-not (memq 'clatter-fool buffer-invisibility-spec)))))

(ert-deftest clatter-test-suppress-cannot-desync-fool-visibility ()
  "Explicit generic suppressions cannot override the fool toggle state."
  (let ((clatter-fools-visible nil))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(clatter-fool muted))
      (clatter-cmd-unsuppress "clatter-fool")
      (should (memq 'clatter-fool buffer-invisibility-spec))))
  (let ((clatter-fools-visible t))
    (with-temp-buffer
      (clatter-mode)
      (setq buffer-invisibility-spec '(muted))
      (clatter-cmd-suppress "clatter-fool")
      (should-not (memq 'clatter-fool buffer-invisibility-spec)))))

;; --- Channel-at-point detection ---

(ert-deftest clatter-test-channel-at-point-hash ()
  "Detects #channel at point."
  (with-temp-buffer
    (insert "hello #emacs world")
    (goto-char 8)  ; on the #
    (should (equal (clatter-ui--channel-at-point) "#emacs"))))

(ert-deftest clatter-test-channel-at-point-middle ()
  "Detects channel when cursor is in the middle."
  (with-temp-buffer
    (insert "see #emacs for help")
    (goto-char 10)  ; on 'a' in emacs
    (should (equal (clatter-ui--channel-at-point) "#emacs"))))

(ert-deftest clatter-test-channel-at-point-ampersand ()
  "Detects &channel prefix."
  (with-temp-buffer
    (insert "join &local")
    (goto-char 6)
    (should (equal (clatter-ui--channel-at-point) "&local"))))

(ert-deftest clatter-test-channel-at-point-none ()
  "Returns nil when no channel at point."
  (with-temp-buffer
    (insert "just normal text")
    (goto-char 5)
    (should-not (clatter-ui--channel-at-point))))

(ert-deftest clatter-test-channel-at-point-hyphen ()
  "Detects channels with hyphens."
  (with-temp-buffer
    (insert "try #system-crafters")
    (goto-char 10)
    (should (equal (clatter-ui--channel-at-point) "#system-crafters"))))

(ert-deftest clatter-test-channel-at-point-start-of-line ()
  "#channel at start of line."
  (with-temp-buffer
    (insert "#emacs is great")
    (goto-char 3)
    (should (equal (clatter-ui--channel-at-point) "#emacs"))))

;; --- Eldoc function ---

(ert-deftest clatter-test-eldoc-channel-with-nicks ()
  "Eldoc shows user count for known channel."
  (let* ((network "testnet")
         (channel "#emacs")
         (buf (clatter-get-or-create-buffer network channel)))
    (unwind-protect
        (progn
          ;; Populate nick list
          (with-current-buffer buf
            (let ((ht (make-hash-table :test 'equal)))
              (puthash "alice" "@" ht)
              (puthash "bob" "" ht)
              (puthash "carol" "+" ht)
              (setq-local clatter--nick-list ht)
              (setq-local clatter--topic "Welcome to #emacs")))
          ;; Simulate being in a clatter buffer on this network
          (with-temp-buffer
            (setq-local clatter--network network)
            (insert "#emacs")
            (goto-char 3)
            (let ((result nil))
              (clatter-ui--eldoc-function
               (lambda (text &rest _) (setq result text)))
              (should result)
              (should (string-match-p "3 users" result))
              (should (string-match-p "Welcome to #emacs" result)))))
      (kill-buffer buf)
      (clatter-remove-buffer network channel))))

(ert-deftest clatter-test-eldoc-channel-no-data ()
  "Eldoc returns nothing for unknown channel."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (insert "#nonexistent")
    (goto-char 3)
    (let ((result nil))
      (clatter-ui--eldoc-function
       (lambda (text &rest _) (setq result text)))
      (should-not result))))

(ert-deftest clatter-test-eldoc-sender ()
  "Eldoc shows sender info on message."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (insert (propertize "hello world"
                        'clatter-sender "alice"
                        'clatter-msgid "msg123"))
    (goto-char 3)
    (let ((result nil))
      (clatter-ui--eldoc-function
       (lambda (text &rest _) (setq result text)))
      (should result)
      (should (string-match-p "alice" result))
      (should (string-match-p "msg123" result)))))

;; --- Header-line ---

(ert-deftest clatter-test-header-line-default-disabled ()
  "Clatter leaves the header line disabled by default."
  (with-temp-buffer
    (clatter-mode)
    (setq-local clatter--network "testnet")
    (setq-local clatter--target "#emacs")
    (clatter-ui-setup-buffer (current-buffer))
    (should-not header-line-format)))

(ert-deftest clatter-test-header-line-renders-channel-context ()
  "Built-in header-line renderer shows full channel context."
  (let ((clatter-header-line-preset 'context))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#emacs")
      (setq-local clatter--topic "A deliberately long topic that is not truncated in the header line")
      (setq-local clatter--channel-modes "+nt")
      (setq-local clatter--nick-list (make-hash-table :test 'equal))
      (puthash "alice" '("" . "alice") clatter--nick-list)
      (puthash "bob" '("@" . "bob") clatter--nick-list)
      (clatter-ui-setup-buffer (current-buffer))
      (should (equal header-line-format
                     '(:eval (clatter--header-line-string))))
      (let ((rendered (clatter--header-line-string)))
        (should (string-match-p "\\[testnet/#emacs\\]" rendered))
        (should (string-match-p "\\+nt" rendered))
        (should (string-match-p "2 nicks" rendered))
        (should (string-match-p "not truncated in the header line" rendered))))))

(ert-deftest clatter-test-header-line-topic-preset-deduplicates-topic ()
  "The topic preset moves only the topic out of the mode-line."
  (let ((clatter-header-line-preset 'topic))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#emacs")
      (setq-local clatter--topic "A full topic")
      (clatter-ui-setup-buffer (current-buffer))
      (should (equal header-line-format
                     '(:eval (clatter--header-line-topic-string))))
      (should (equal (clatter--header-line-topic-string) "A full topic"))
      (let ((mode-line (clatter--mode-line-string)))
        (should (string-match-p "\\[testnet/#emacs\\]" mode-line))
        (should-not (string-match-p "A full topic" mode-line))))))

(ert-deftest clatter-test-header-line-context-preset-deduplicates-context ()
  "The context preset leaves only the current nick in the mode-line."
  (let ((clatter-header-line-preset 'context))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#emacs")
      (setq-local clatter--topic "A full topic")
      (setq-local clatter--channel-modes "+nt")
      (setq-local clatter--nick-list (make-hash-table :test 'equal))
      (puthash "alice" t clatter--nick-list)
      (clatter-ui-setup-buffer (current-buffer))
      (should (equal header-line-format
                     '(:eval (clatter--header-line-string))))
      (should-not (memq 'mode-line-buffer-identification mode-line-format))
      (let ((mode-line (clatter--mode-line-string)))
        (should (string-match-p "\\?" mode-line))
        (should-not (string-match-p "testnet/#emacs" mode-line))
        (should-not (string-match-p "1" mode-line))
        (should-not (string-match-p "A full topic" mode-line))))))
;; --- Read state ---

(ert-deftest clatter-test-read-state-clear-persists-and-restores ()
  "Clearing activity persists and restores the latest read timestamp."
  (let* ((file (make-temp-file "clatter-read-state"))
         (clatter-read-state-enabled t)
         (clatter-read-state-file file)
         (clatter-read-state--table (make-hash-table :test 'equal))
         (clatter-read-state--loaded t)
         (clatter-read-state--save-timer nil)
         (time (encode-time 0 0 12 1 1 2026 t)))
    (unwind-protect
        (progn
          (with-temp-buffer
            (clatter-mode)
            (setq-local clatter--network "libera")
            (setq-local clatter--target "#emacs")
            (setq-local clatter--latest-message-time time)
            (clatter-clear-activity (current-buffer)))
          (clatter-read-state--save-now)
          (setq clatter-read-state--table (make-hash-table :test 'equal))
          (setq clatter-read-state--loaded nil)
          (with-temp-buffer
            (clatter-mode)
            (setq-local clatter--network "libera")
            (setq-local clatter--target "#emacs")
            (clatter-read-state-restore-buffer (current-buffer))
            (should (equal clatter--last-read-time time))))
      (when clatter-read-state--save-timer
        (cancel-timer clatter-read-state--save-timer))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest clatter-test-read-state-suppresses-read-history-activity ()
  "History at or before the last-read timestamp does not mark activity."
  (let ((conn (clatter-test-make-connection "libera" "me"))
        (buf nil)
        (clatter-read-state-enabled t))
    (unwind-protect
        (let* ((last-read (encode-time 0 0 12 1 1 2026 t))
               (old (encode-time 0 59 11 1 1 2026 t))
               (equal-time (encode-time 0 0 12 1 1 2026 t))
               (new (encode-time 0 1 12 1 1 2026 t)))
          (setq buf (clatter-get-or-create-buffer "libera" "#emacs" 'channel))
          (with-current-buffer buf
            (clatter-ui-setup-buffer buf)
            (setq-local clatter--last-read-time last-read))
          (with-temp-buffer
            (clatter-insert-privmsg buf "alice" "old" conn old)
            (clatter-insert-privmsg buf "alice" "equal" conn equal-time)
            (clatter-insert-privmsg buf "alice" "new" conn new))
          (with-current-buffer buf
            (should (= clatter--unread-count 1)))
          (let* ((infos (clatter-track--collect))
                 (info (cl-find buf infos
                                :key (lambda (entry)
                                       (plist-get entry :buffer)))))
            (should info)
            (should (= (plist-get info :unread) 1))))
      (clatter-test-cleanup)
      (when buf
        (clatter-remove-buffer "libera" "#emacs")
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest clatter-test-backlog-page-renders-at-oldest-end ()
  "A `clatter-chathistory-more' page lands past existing history.
Both message orders keep the page's own messages in display order."
  (dolist (order '(newest-first oldest-first))
    (let ((clatter-message-order order)
          (conn (clatter-test-make-connection "testnet" "me"))
          (buf nil))
      (unwind-protect
          (progn
            (setq buf (clatter-get-or-create-buffer "testnet" "#chan" 'channel))
            (with-current-buffer buf (clatter-ui-setup-buffer buf))
            (clatter-ui--on-batch-complete
             conn "chathistory" "#chan"
             (list (list :type 'privmsg :sender "a" :text "old1"
                         :time (encode-time 0 0 10 1 1 2026 t))
                   (list :type 'privmsg :sender "a" :text "old2"
                         :time (encode-time 0 0 11 1 1 2026 t))))
            (with-current-buffer buf
              (setq-local clatter--backlog-page-pending t))
            (clatter-ui--on-batch-complete
             conn "chathistory" "#chan"
             (list (list :type 'privmsg :sender "c" :text "older1"
                         :time (encode-time 0 0 8 1 1 2026 t))
                   (list :type 'privmsg :sender "c" :text "older2"
                         :time (encode-time 0 0 9 1 1 2026 t))))
            (with-current-buffer buf
              (let ((texts nil))
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward "\\(older[0-9]\\|old[0-9]\\)" nil t)
                    (push (match-string 1) texts)))
                (should (equal (nreverse texts)
                               (if (eq order 'newest-first)
                                   '("old2" "old1" "older2" "older1")
                                 '("older1" "older2" "old1" "old2"))))
                (should-not clatter--backlog-page-pending))))
        (clatter-test-cleanup)
        (when buf
          (clatter-remove-buffer "testnet" "#chan")
          (when (buffer-live-p buf) (kill-buffer buf)))))))

(ert-deftest clatter-test-batch-playback-renders-read-history-without-activity ()
  "Batch playback renders seen messages without marking them unread."
  (let ((conn (clatter-test-make-connection "libera" "me"))
        (buf nil)
        (clatter-read-state-enabled t)
        (clatter-read-state--loaded t)
        (clatter-read-state--table (make-hash-table :test 'equal)))
    (unwind-protect
        (let ((last-read (encode-time 0 0 12 1 1 2026 t))
              (old (encode-time 0 59 11 1 1 2026 t))
              (new (encode-time 0 1 12 1 1 2026 t)))
          (setq buf (clatter-get-or-create-buffer "libera" "#emacs" 'channel))
          (with-current-buffer buf
            (clatter-ui-setup-buffer buf)
            (setq-local clatter--last-read-time last-read))
          (clatter-ui--on-batch-complete
           conn "chathistory" "#emacs"
           (list (list :type 'privmsg :sender "alice"
                       :text "me: older message" :time old)
                 (list :type 'privmsg :sender "carol"
                       :text "last-read message" :time last-read)
                 (list :type 'privmsg :sender "bob"
                       :text "new message" :time new)))
          (with-current-buffer buf
            (let ((rendered (buffer-string)))
              (should (string-match-p "me: older message" rendered))
              (should (string-match-p "last-read message" rendered))
              (should (string-match-p "new message" rendered))
              (should (string-match-p "end of history (3 messages)" rendered))
              (should (= clatter--unread-count 1))
              (should-not clatter--has-mention))))
      (clatter-test-cleanup)
      (when buf
        (clatter-remove-buffer "libera" "#emacs")
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest clatter-test-divider-stretches-to-window ()
  "The bar centers its label and keeps both rules through message filling."
  (let ((bar (clatter--divider "history")))
    (should (string-match-p "history" bar))
    (should (pcase (get-text-property 0 'display bar)
              (`(space :align-to (- center ,(pred integerp))) t)))
    (let ((clatter-fill-column 80)
          (clatter-nick-column-width 7))
      (with-temp-buffer
        (clatter--insert-message (current-buffer) bar t)
        (goto-char (point-min))
        (end-of-line)
        (should (equal '(space :align-to right)
                       (get-text-property (1- (point)) 'display)))))))

(ert-deftest clatter-test-motd-uses-window-dividers ()
  "MOTD boundaries use window-wide dividers."
  (clatter-test-with-ui-connection conn
    (let (labels)
      (cl-letf (((symbol-function 'clatter--divider)
                 (lambda (label)
                   (push label labels)
                   label)))
        (clatter-ui--on-motd conn '("Welcome")))
      (should (equal (nreverse labels) '("MOTD" "End of MOTD"))))))

;; --- Typing indicators ---

(ert-deftest clatter-test-typing-location-default-keeps-mode-line-layout ()
  "The default keeps typing and configured activity in the mode line."
  (let ((clatter-typing-indicator-location 'mode-line)
        (clatter-track-show-in-clatter-buffers t))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#emacs")
      (setq-local clatter--buffer-type 'channel)
      (clatter-ui-setup-buffer (current-buffer))
      (should (member '(:eval (clatter--typing-mode-line)) mode-line-format))
      (should (memq 'clatter-track-mode-line-item mode-line-format))
      (should-not clatter--typing-indicator-overlay))))

(ert-deftest clatter-test-typing-location-separator-leaves-track-fixed ()
  "The separator placement omits typing from the mode line, not activity."
  (let ((clatter-typing-indicator-location 'input-separator)
        (clatter-track-show-in-clatter-buffers t))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#emacs")
      (setq-local clatter--buffer-type 'channel)
      (clatter-ui-setup-buffer (current-buffer))
      (should-not (member '(:eval (clatter--typing-mode-line)) mode-line-format))
      (should (memq 'clatter-track-mode-line-item mode-line-format))
      (should (overlayp clatter--typing-indicator-overlay)))))

(ert-deftest clatter-test-typing-separator-conversation-buffers-only ()
  "Channels and queries reserve typing rows; server buffers do not."
  (let ((clatter-typing-indicator-location 'input-separator))
    (dolist (case '((channel "#emacs" t)
                    (query "alice" t)
                    (server "*server*" nil)))
      (with-temp-buffer
        (clatter-mode)
        (setq-local clatter--network "testnet")
        (setq-local clatter--target (cadr case))
        (setq-local clatter--buffer-type (car case))
        (clatter-ui-setup-buffer (current-buffer))
        (should (eq (and (overlayp clatter--typing-indicator-overlay) t)
                    (nth 2 case)))))))

(ert-deftest clatter-test-typing-separator-aligns-all-wording ()
  "One, two, and many typing states align with the message body."
  (let ((clatter-typing-indicator-location 'input-separator)
        (clatter-nick-column-width 12))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--target "#emacs")
      (setq-local clatter--buffer-type 'channel)
      (clatter--setup-prompt (current-buffer))
      (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
      (dolist (case '(("alice is typing" "alice")
                      ("are typing" "alice" "bob")
                      ("3 people typing" "alice" "bob" "carol")))
        (clrhash clatter--typing-nicks)
        (dolist (nick (cdr case))
          (puthash nick t clatter--typing-nicks))
        (clatter--refresh-typing-indicator)
        (let ((text (overlay-get clatter--typing-indicator-overlay
                                 'before-string)))
          (should (string-prefix-p (make-string 13 ?\s) text))
          (should (string-match-p (car case) text)))))))

(ert-deftest clatter-test-typing-separator-clears-on-done-and-expiry ()
  "Done and timeout clear separator text without removing its row."
  (let ((clatter-typing-indicator-location 'input-separator)
        (conn (clatter-test-make-connection "typing-test" "me"))
        buffer)
    (unwind-protect
        (progn
          (setq buffer (clatter-get-or-create-buffer
                        "typing-test" "#chat" 'channel))
          (with-current-buffer buffer
            (clatter-ui-setup-buffer buffer))
          (clatter-ui--on-typing conn '("alice" nil nil) "#chat" "active")
          (with-current-buffer buffer
            (should (string-match-p
                     "alice is typing"
                     (overlay-get clatter--typing-indicator-overlay
                                  'before-string))))
          (clatter-ui--on-typing conn '("alice" nil nil) "#chat" "done")
          (with-current-buffer buffer
            (should-not (overlay-get clatter--typing-indicator-overlay
                                     'before-string))
            (should (overlay-buffer clatter--typing-indicator-overlay)))
          (clatter-ui--on-typing conn '("alice" nil nil) "#chat" "paused")
          (with-current-buffer buffer
            (cancel-timer (gethash "alice" clatter--typing-nicks)))
          (clatter--expire-typing-indicator buffer "alice")
          (with-current-buffer buffer
            (should-not (overlay-get clatter--typing-indicator-overlay
                                     'before-string))
            (should (overlay-buffer clatter--typing-indicator-overlay))))
      (clatter-test-cleanup)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest clatter-test-typing-mode-line-empty ()
  "No typing nicks returns nil."
  (with-temp-buffer
    (setq-local clatter--typing-nicks nil)
    (should-not (clatter--typing-mode-line))))

(ert-deftest clatter-test-typing-mode-line-one ()
  "One nick typing shows name."
  (with-temp-buffer
    (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
    (puthash "alice" t clatter--typing-nicks)
    (let ((result (clatter--typing-mode-line)))
      (should result)
      (should (string-match-p "alice is typing" result)))))

(ert-deftest clatter-test-typing-mode-line-two ()
  "Two nicks typing shows both names."
  (with-temp-buffer
    (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
    (puthash "alice" t clatter--typing-nicks)
    (puthash "bob" t clatter--typing-nicks)
    (let ((result (clatter--typing-mode-line)))
      (should result)
      (should (string-match-p "are typing" result)))))

(ert-deftest clatter-test-typing-mode-line-many ()
  "Three+ nicks shows count."
  (with-temp-buffer
    (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
    (puthash "alice" t clatter--typing-nicks)
    (puthash "bob" t clatter--typing-nicks)
    (puthash "carol" t clatter--typing-nicks)
    (let ((result (clatter--typing-mode-line)))
      (should result)
      (should (string-match-p "3 people typing" result)))))

;; --- Outbound typing throttle ---

(ert-deftest clatter-test-typing-capable-no-network ()
  "Not typing-capable without network."
  (with-temp-buffer
    (setq-local clatter--network nil)
    (setq-local clatter--target "#test")
    (should-not (clatter--typing-capable-p))))

(ert-deftest clatter-test-typing-capable-no-target ()
  "Not typing-capable without target."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (setq-local clatter--target nil)
    (should-not (clatter--typing-capable-p))))

(ert-deftest clatter-test-typing-capable-server-buffer ()
  "Not typing-capable in server buffer."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (setq-local clatter--target "*server*")
    (should-not (clatter--typing-capable-p))))

(ert-deftest clatter-test-typing-capable-disabled ()
  "Not typing-capable when disabled."
  (with-temp-buffer
    (setq-local clatter--network "testnet")
    (setq-local clatter--target "#test")
    (let ((clatter-send-typing nil))
      (should-not (clatter--typing-capable-p)))))

(ert-deftest clatter-test-typing-capable-with-caps ()
  "Typing-capable when message-tags enabled."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (with-temp-buffer
          (setq-local clatter--network "testnet")
          (setq-local clatter--target "#test")
          (let ((clatter-send-typing t))
            (should (clatter--typing-capable-p))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-typing-not-capable-when-tagmsg-rejected ()
  "Typing suppressed once the server returned 421 for TAGMSG."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (setf (clatter-connection-tagmsg-rejected conn) t)
          (with-temp-buffer
            (setq-local clatter--network "testnet")
            (setq-local clatter--target "#test")
            (let ((clatter-send-typing t))
              (should-not (clatter--typing-capable-p)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-numeric-421-tagmsg-sets-flag-and-displays ()
  "421 for TAGMSG sets the connection flag and still prints to the server buffer."
  (clatter-test-with-ui-connection conn
    (let (inserted)
      (clatter-get-or-create-buffer "testnet" "*server*" 'server)
      (cl-letf (((symbol-function 'clatter-insert-system)
                 (lambda (buffer text &optional _invisible)
                   (push (cons buffer text) inserted))))
        (clatter-ui--on-numeric conn "421"
                                 '("testnick" "TAGMSG" "Unknown command")))
      (should (clatter-connection-tagmsg-rejected conn))
      (let ((server (clatter-get-server-buffer "testnet")))
        (should server)
        (should (cl-find-if (lambda (cell)
                              (and (eq (car cell) server)
                                   (string-match-p "\\[421\\].*TAGMSG"
                                                   (cdr cell))))
                            inserted))))))

(ert-deftest clatter-test-numeric-421-other-command-no-flag ()
  "421 for a non-TAGMSG command does not set the tagmsg-rejected flag."
  (clatter-test-with-ui-connection conn
    (clatter-get-or-create-buffer "testnet" "*server*" 'server)
    (cl-letf (((symbol-function 'clatter-insert-system)
               (lambda (_buffer _text &optional _invisible))))
      (clatter-ui--on-numeric conn "421"
                               '("testnick" "FOOBAR" "Unknown command")))
    (should-not (clatter-connection-tagmsg-rejected conn))))

(ert-deftest clatter-test-numeric-query-reply-routes-to-origin-buffer ()
  "A query-reply numeric routes to the buffer that issued the command.
The process filter may run in an unrelated buffer, so routing must not
rely on `current-buffer'."
  (clatter-test-with-ui-connection conn
    (let (inserted)
      (let ((chan (clatter-get-or-create-buffer "testnet" "#test" 'channel)))
        (setf (clatter-connection-last-query-buffer conn) chan)
        (clatter-get-or-create-buffer "testnet" "*server*" 'server)
        (with-temp-buffer
          (cl-letf (((symbol-function 'clatter-insert-system)
                     (lambda (buffer text &optional _invisible)
                       (push (cons buffer text) inserted))))
            (clatter-ui--on-numeric conn "352"
                                     '("testnick" "#test" "user" "host"
                                       "server" "nick" "H" "0 real"))))
        (should (cl-find-if (lambda (cell) (eq (car cell) chan)) inserted))
        (should-not (cl-find-if (lambda (cell)
                                  (not (eq (car cell) chan)))
                                inserted))))))

(ert-deftest clatter-test-numeric-query-reply-falls-to-server-when-no-origin ()
  "Without a recorded origin (e.g. the welcome burst), query-reply
numerics fall through to the server buffer."
  (clatter-test-with-ui-connection conn
    (let (inserted)
      (clatter-get-or-create-buffer "testnet" "*server*" 'server)
      (cl-letf (((symbol-function 'clatter-insert-system)
                 (lambda (buffer text &optional _invisible)
                   (push (cons buffer text) inserted))))
        (clatter-ui--on-numeric conn "251"
                                 '("testnick" "3 4" "There are 3 users")))
      (let ((server (clatter-get-server-buffer "testnet")))
        (should server)
        (should (cl-find-if (lambda (cell) (eq (car cell) server))
                            inserted))))))

(ert-deftest clatter-test-internal-whox-replies-not-displayed ()
  "Replies to the automatic per-channel WHOX are consumed silently.
`clatter-send-whox' fires on every RPL_ENDOFNAMES, so displaying its 315s
floods the buffer with \"End of /WHO list\" on every rejoin."
  (clatter-test-with-ui-connection conn
    (let ((inserted nil)
          (chans '("#erc" "#ircv3" "#guix")))
      (clatter-get-or-create-buffer "testnet" "*server*" 'server)
      (setf (clatter-connection-last-query-buffer conn)
            (clatter-get-or-create-buffer "testnet" "#clatter" 'channel))
      (cl-letf (((symbol-function 'clatter-insert-system)
                 (lambda (buffer text &optional _invisible)
                   (push (cons buffer text) inserted)))
                ((symbol-function 'clatter-send) (lambda (&rest _) nil)))
        (dolist (c chans) (clatter-send-whox conn c))
        (dolist (c chans)
          (clatter-ui--on-numeric conn "315"
                                   (list "testnick" c "End of /WHO list"))))
      (should-not inserted)
      ;; Each terminating 315 clears its pending entry.
      (should-not (clatter-connection-pending-whox conn)))))

(ert-deftest clatter-test-query-routing-cleared-by-end-numeric ()
  "A terminating numeric stops query routing so it is not sticky.
Otherwise every later unsolicited reply keeps landing in whichever buffer
last ran a query command."
  (clatter-test-with-ui-connection conn
    (let ((inserted nil))
      (clatter-get-or-create-buffer "testnet" "*server*" 'server)
      (let ((qb (clatter-get-or-create-buffer "testnet" "#clatter" 'channel)))
        (setf (clatter-connection-last-query-buffer conn) qb)
        (cl-letf (((symbol-function 'clatter-insert-system)
                   (lambda (buffer text &optional _invisible)
                     (push (cons buffer text) inserted))))
          ;; A user /who: body then terminator, both shown in the origin.
          (clatter-ui--on-numeric conn "352"
                                   '("testnick" "#clatter" "u" "h" "s"
                                     "nick" "H" "0 real"))
          (clatter-ui--on-numeric conn "315"
                                   '("testnick" "#clatter" "End of /WHO list"))
          (should (cl-every (lambda (cell) (eq (car cell) qb)) inserted))
          (should (null (clatter-connection-last-query-buffer conn)))
          ;; A later unsolicited 315 now goes to the server buffer.
          (setq inserted nil)
          (clatter-ui--on-numeric conn "315"
                                   '("testnick" "#erc" "End of /WHO list"))
          (should (eq (car (car inserted))
                      (clatter-get-server-buffer "testnet"))))))))

;; --- Notifications and read state ---

(ert-deftest clatter-test-read-state-suppresses-read-notification ()
  "Messages at or before the last-read timestamp do not notify."
  (let ((clatter-notify-enabled t)
        (clatter-notify-on-mention t)
        (clatter-notify-current-buffer t)
        (clatter-notify-cooldown 0)
        (clatter-read-state-enabled t)
        (clatter--buffer-alist nil)
        (clatter-notify--last-times (make-hash-table :test 'equal))
        (sent nil)
        (conn (clatter-test-make-connection "testnet" "trev"))
        (last-read (encode-time 0 0 12 1 1 2026 t)))
    (unwind-protect
        (let ((buf (clatter-get-or-create-buffer "testnet" "#test")))
          (with-current-buffer buf
            (setq-local clatter--last-read-time last-read))
          (cl-letf (((symbol-function 'clatter-notify--send)
                     (lambda (title body &optional _buffer)
                       (push (list title body) sent))))
            (clatter-notify--on-privmsg
             conn '("alice" nil nil) "#test" "trev: already saw this" last-read)
            (should-not sent)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-read-state-allows-unread-notification ()
  "Messages after the last-read timestamp can still notify."
  (let ((clatter-notify-enabled t)
        (clatter-notify-on-mention t)
        (clatter-notify-current-buffer t)
        (clatter-notify-cooldown 0)
        (clatter-read-state-enabled t)
        (clatter--buffer-alist nil)
        (clatter-notify--last-times (make-hash-table :test 'equal))
        (sent nil)
        (conn (clatter-test-make-connection "testnet" "trev"))
        (last-read (encode-time 0 0 12 1 1 2026 t))
        (unread-time (encode-time 1 0 12 1 1 2026 t)))
    (unwind-protect
        (let ((buf (clatter-get-or-create-buffer "testnet" "#test")))
          (with-current-buffer buf
            (setq-local clatter--last-read-time last-read))
          (cl-letf (((symbol-function 'clatter-notify--send)
                     (lambda (title body &optional _buffer)
                       (push (list title body) sent))))
            (clatter-notify--on-privmsg
             conn '("alice" nil nil) "#test" "trev: new message" unread-time)
            (should sent)))
      (clatter-test-cleanup))))

;; --- Nicklist hooks registered ---

(ert-deftest clatter-test-nicklist-hooks-registered ()
  "Nicklist auto-refresh and follow hooks are registered."
  (should (memq #'clatter-nicklist--on-join (default-value 'clatter-join-hook)))
  (should (memq #'clatter-nicklist--on-part (default-value 'clatter-part-hook)))
  (should (memq #'clatter-nicklist--on-quit (default-value 'clatter-quit-hook)))
  (should (memq #'clatter-nicklist--on-nick (default-value 'clatter-nick-hook)))
  (should (memq #'clatter-nicklist--on-names (default-value 'clatter-names-hook)))
  (should (memq #'clatter-nicklist--follow
              (default-value 'window-buffer-change-functions)))
  (should (memq #'clatter-nicklist--follow
              (default-value 'window-selection-change-functions))))

(ert-deftest clatter-test-nicklist-follows-selected-channel ()
  "An open nicklist retargets when another channel is selected."
  (clatter-test-with-ui-connection conn
    (let ((a (clatter-get-or-create-buffer "testnet" "#a" 'channel))
          (b (clatter-get-or-create-buffer "testnet" "#b" 'channel))
          (nl nil))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (clatter-nick-add a "alice")
            (clatter-nick-add b "bob")
            (let* ((chan-win (selected-window))
                   (nl-win (split-window-right)))
              (setq nl (get-buffer-create clatter-nicklist--buffer-name))
              (with-current-buffer nl
                (clatter-nicklist-mode)
                (setq clatter-nicklist--source-buffer a)
                (clatter-nicklist--render a))
              (set-window-buffer chan-win a)
              (set-window-buffer nl-win nl)
              (select-window chan-win)
              (set-window-buffer chan-win b)
              (clatter-nicklist--follow (selected-frame))
              (should (eq (buffer-local-value 'clatter-nicklist--source-buffer nl) b))
              (with-current-buffer nl
                (goto-char (point-min))
                (should (search-forward "bob" nil t))
                (goto-char (point-min))
                (should-not (search-forward "alice" nil t)))))
        (when (buffer-live-p nl) (kill-buffer nl))))))

(ert-deftest clatter-test-nicklist-follow-does-not-open ()
  "Follow does not create a nicklist when none is showing."
  (clatter-test-with-ui-connection conn
    (let ((a (clatter-get-or-create-buffer "testnet" "#a" 'channel)))
      (when-let* ((stale (get-buffer clatter-nicklist--buffer-name)))
        (kill-buffer stale))
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (selected-window) a)
        (clatter-nicklist--follow (selected-frame))
        (should-not (get-buffer clatter-nicklist--buffer-name))))))

(ert-deftest clatter-test-nicklist-follow-ignores-query ()
  "Selecting a query leaves the last channel nicklist in place."
  (clatter-test-with-ui-connection conn
    (let ((a (clatter-get-or-create-buffer "testnet" "#a" 'channel))
          (q (clatter-get-or-create-buffer "testnet" "alice" 'query))
          (nl nil))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (clatter-nick-add a "alice")
            (let* ((chan-win (selected-window))
                   (nl-win (split-window-right)))
              (setq nl (get-buffer-create clatter-nicklist--buffer-name))
              (with-current-buffer nl
                (clatter-nicklist-mode)
                (setq clatter-nicklist--source-buffer a)
                (clatter-nicklist--render a))
              (set-window-buffer chan-win a)
              (set-window-buffer nl-win nl)
              (select-window chan-win)
              (set-window-buffer chan-win q)
              (clatter-nicklist--follow (selected-frame))
              (should (eq (buffer-local-value 'clatter-nicklist--source-buffer nl) a))
              (with-current-buffer nl
                (goto-char (point-min))
                (should (search-forward "alice" nil t)))))
        (when (buffer-live-p nl) (kill-buffer nl))))))

(ert-deftest clatter-test-nicklist-auto-refresh-stays-on-source ()
  "Membership events on another channel do not retarget the list."
  (clatter-test-with-ui-connection conn
    (let ((a (clatter-get-or-create-buffer "testnet" "#a" 'channel))
          (b (clatter-get-or-create-buffer "testnet" "#b" 'channel))
          (nl nil))
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (clatter-nick-add a "alice")
            (clatter-nick-add b "bob")
            (let ((nl-win (split-window-right)))
              (setq nl (get-buffer-create clatter-nicklist--buffer-name))
              (with-current-buffer nl
                (clatter-nicklist-mode)
                (setq clatter-nicklist--source-buffer a)
                (clatter-nicklist--render a))
              (set-window-buffer (selected-window) a)
              (set-window-buffer nl-win nl)
              (clatter-nicklist--auto-refresh b)
              (should (eq (buffer-local-value 'clatter-nicklist--source-buffer nl) a))
              (with-current-buffer nl
                (goto-char (point-min))
                (should (search-forward "alice" nil t))
                (goto-char (point-min))
                (should-not (search-forward "bob" nil t)))))
        (when (buffer-live-p nl) (kill-buffer nl))))))

;; --- Numeric reply routing ---

(ert-deftest clatter-test-numeric-unhandled-goes-to-server ()
  "An unhandled numeric (e.g. 421) prints to the server buffer, not dropped."
  (clatter-test-with-ui-connection conn
    (let (inserted)
      (clatter-get-or-create-buffer "testnet" "*server*" 'server)
      (cl-letf (((symbol-function 'clatter-insert-system)
                 (lambda (buffer text &optional _invisible)
                   (push (cons buffer text) inserted))))
        (clatter-ui--on-numeric conn "421"
                                 '("testnick" "FOOBAR" "Unknown command")))
      (let ((server (clatter-get-server-buffer "testnet")))
        (should server)
        (should (cl-find-if (lambda (cell)
                              (and (eq (car cell) server)
                                   (string-match-p "\\[421\\]" (cdr cell))))
                            inserted))))))

(ert-deftest clatter-test-numeric-403-routes-to-channel-buffer ()
  "403 routes to the param-derived channel buffer, not (current-buffer)."
  (clatter-test-with-ui-connection conn
    (let (inserted)
      (let ((chan (clatter-get-or-create-buffer "testnet" "#gone" 'channel)))
        (with-temp-buffer
          ;; current-buffer is a non-clatter temp buffer at filter time
          (cl-letf (((symbol-function 'clatter-insert-system)
                     (lambda (buffer text &optional _invisible)
                       (push (cons buffer text) inserted))))
            (clatter-ui--on-numeric conn "403"
                                     '("testnick" "#gone" "No such channel"))))
        (should (cl-find-if (lambda (cell) (eq (car cell) chan)) inserted))
        ;; The temp buffer must never have been the target.
        (should-not (cl-find-if (lambda (cell)
                                  (not (eq (car cell) chan)))
                                inserted))))))

;;; Nick-grouping helpers

(defun clatter-test--find-line-bol (buffer text)
  "Return the bol of the line in BUFFER whose text contains TEXT, or nil."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (when (search-forward text nil t)
        (line-beginning-position)))))

(defun clatter-test--nick-column-at (buffer bol)
  "Return the rendered nick column at BOL in BUFFER."
  (with-current-buffer buffer
    (let* ((line-end (save-excursion
                       (goto-char bol)
                       (line-end-position)))
           (end (next-single-property-change
                 bol 'clatter-nick-column nil line-end))
           (display (get-text-property bol 'display)))
      (if (get-text-property bol 'clatter-grouped)
          display
        (buffer-substring-no-properties bol end)))))

(defun clatter-test--line-spacing-at (buffer bol)
  "Return the line-spacing property on the newline after BOL in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char bol)
      (get-text-property (line-end-position) 'line-spacing))))

(defmacro clatter-test--with-grouping-buffer (order conn &rest body)
  "Run BODY in a fresh clatter buffer with nick grouping setup.
ORDER is the `clatter-message-order', CONN the mock connection."
  (declare (indent 2))
  `(with-temp-buffer
     (let ((clatter-message-order ,order))
       (clatter-mode)
       (setq-local clatter--network "testnet")
       (setq-local clatter--target "#test")
       (setq-local clatter--buffer-type 'channel)
       (clatter-ui-setup-buffer (current-buffer))
       ,@body)))

(ert-deftest clatter-group-messages-blanks-consecutive-same-nick ()
  "A burst of same-nick PRIVMSGs shows the nick once; later lines are blanked."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-group-messages-by-nick t)
        (clatter-typing-indicator-location 'input-separator))
    (unwind-protect
        (dolist (order '(oldest-first newest-first))
          (clatter-test--with-grouping-buffer order conn
            (clatter-insert-privmsg (current-buffer) "alice" "first" conn)
            (clatter-insert-privmsg (current-buffer) "alice" "second" conn)
            (let ((first-bol (clatter-test--find-line-bol (current-buffer) "first"))
                  (second-bol (clatter-test--find-line-bol (current-buffer) "second")))
              (should first-bol)
              (should second-bol)
              ;; Chronologically-first keeps the nick; "second" is blanked.
              (should (string-match-p "<alice>"
                                      (clatter-test--nick-column-at
                                       (current-buffer) first-bol)))
              (should (string-match-p "\\` *\\'"
                                      (clatter-test--nick-column-at
                                       (current-buffer) second-bol)))
              ;; Sender metadata survives on the blanked line.
              (should (equal (get-text-property second-bol 'clatter-sender) "alice"))
              (should (eq (get-text-property second-bol 'clatter-msg-type) 'privmsg)))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-group-messages-adds-gap-between-groups ()
  "Nick groups have fractional line spacing between them in either order."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-group-messages-by-nick t)
        (clatter-group-messages-gap 0.25))
    (unwind-protect
        (dolist (order '(oldest-first newest-first))
          (clatter-test--with-grouping-buffer order conn
            (clatter-insert-privmsg (current-buffer) "alice" "first" conn)
            (clatter-insert-privmsg (current-buffer) "alice" "second" conn)
            (clatter-insert-privmsg (current-buffer) "bob" "third" conn)
            (let ((first-bol (clatter-test--find-line-bol (current-buffer) "first"))
                  (second-bol (clatter-test--find-line-bol (current-buffer) "second"))
                  (third-bol (clatter-test--find-line-bol (current-buffer) "third")))
              (should-not (clatter-test--line-spacing-at
                           (current-buffer) first-bol))
              (should (equal (clatter-test--line-spacing-at
                              (current-buffer)
                              (if (eq order 'oldest-first) second-bol third-bol))
                             0.25))
              (should-not (clatter-test--line-spacing-at
                           (current-buffer)
                           (if (eq order 'oldest-first) third-bol second-bol))))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-group-messages-regroups-when-fools-toggle ()
  "Visible nick groups and gaps follow fool visibility in either order."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-group-messages-by-nick t)
        (clatter-group-messages-gap 0.25)
        (clatter-fools-visible nil))
    (unwind-protect
        (dolist (order '(oldest-first newest-first))
          (clatter-test--with-grouping-buffer order conn
            (clatter-insert-privmsg (current-buffer) "alice" "first" conn)
            (clatter-insert-privmsg (current-buffer) "fool" "foolish" conn
                                    nil 'clatter-fool)
            (clatter-insert-privmsg (current-buffer) "alice" "second" conn)
            (let ((first-bol
                   (clatter-test--find-line-bol (current-buffer) "first"))
                  (fool-bol
                   (clatter-test--find-line-bol (current-buffer) "foolish"))
                  (second-bol
                   (clatter-test--find-line-bol (current-buffer) "second")))
              ;; Hidden fools do not interrupt the visible group.
              (should (string-match-p
                       "\\` *\\'"
                       (clatter-test--nick-column-at
                        (current-buffer) second-bol)))
              (dolist (bol (list first-bol fool-bol second-bol))
                (should-not
                 (clatter-test--line-spacing-at (current-buffer) bol)))
              ;; Revealing fools recomputes both nick columns and gaps.
              (clatter-toggle-fools 1)
              (setq first-bol
                    (clatter-test--find-line-bol (current-buffer) "first")
                    fool-bol
                    (clatter-test--find-line-bol (current-buffer) "foolish")
                    second-bol
                    (clatter-test--find-line-bol (current-buffer) "second"))
              (should (string-match-p
                       "<fool>"
                       (clatter-test--nick-column-at
                        (current-buffer) fool-bol)))
              (should (string-match-p
                       "<alice>"
                       (clatter-test--nick-column-at
                        (current-buffer) second-bol)))
              (dolist (bol (if (eq order 'oldest-first)
                               (list first-bol fool-bol)
                             (list fool-bol second-bol)))
                (should (equal
                         (clatter-test--line-spacing-at
                          (current-buffer) bol)
                         0.25)))
              ;; Hiding fools restores the original visible group.
              (clatter-toggle-fools -1)
              (setq first-bol
                    (clatter-test--find-line-bol (current-buffer) "first")
                    fool-bol
                    (clatter-test--find-line-bol (current-buffer) "foolish")
                    second-bol
                    (clatter-test--find-line-bol (current-buffer) "second"))
              (should (string-match-p
                       "\\` *\\'"
                       (clatter-test--nick-column-at
                        (current-buffer) second-bol)))
              (dolist (bol (list first-bol fool-bol second-bol))
                (should-not
                 (clatter-test--line-spacing-at (current-buffer) bol))))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-group-messages-gap-visible-fools-anchor ()
  "With fools visible, fool messages anchor grouping and gaps normally."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-group-messages-by-nick t)
        (clatter-group-messages-gap 0.25)
        (clatter-fools-visible t))
    (unwind-protect
        (clatter-test--with-grouping-buffer 'oldest-first conn
          (clatter-insert-privmsg (current-buffer) "alice" "first" conn)
          (clatter-insert-privmsg (current-buffer) "fool" "foolish" conn
                                  nil 'clatter-fool)
          (let ((first-bol (clatter-test--find-line-bol (current-buffer) "first"))
                (fool-bol (clatter-test--find-line-bol (current-buffer) "foolish")))
            ;; The fool broke alice's burst: gap below alice's line.
            (should (equal (clatter-test--line-spacing-at
                            (current-buffer) first-bol)
                           0.25))
            (should (string-match-p "<fool>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) fool-bol)))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-group-messages-breaks-on-different-nick ()
  "An intervening message from another nick breaks the burst."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-group-messages-by-nick t))
    (unwind-protect
        (clatter-test--with-grouping-buffer 'newest-first conn
          (clatter-insert-privmsg (current-buffer) "alice" "first" conn)
          (clatter-insert-privmsg (current-buffer) "bob" "second" conn)
          (clatter-insert-privmsg (current-buffer) "alice" "third" conn)
          (let ((first-bol (clatter-test--find-line-bol (current-buffer) "first"))
                (second-bol (clatter-test--find-line-bol (current-buffer) "second"))
                (third-bol (clatter-test--find-line-bol (current-buffer) "third")))
            (should (string-match-p "<alice>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) first-bol)))
            (should (string-match-p "<bob>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) second-bol)))
            ;; "third" is not blanked: bob broke alice's burst.
            (should (string-match-p "<alice>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) third-bol)))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-group-messages-breaks-on-action ()
  "An action between same-nick PRIVMSGs breaks the burst."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-group-messages-by-nick t))
    (unwind-protect
        (clatter-test--with-grouping-buffer 'newest-first conn
          (clatter-insert-privmsg (current-buffer) "alice" "first" conn)
          (clatter-insert-action (current-buffer) "alice" "waves" conn)
          (clatter-insert-privmsg (current-buffer) "alice" "third" conn)
          (let ((first-bol (clatter-test--find-line-bol (current-buffer) "first"))
                (third-bol (clatter-test--find-line-bol (current-buffer) "third")))
            (should (string-match-p "<alice>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) first-bol)))
            ;; The action broke the burst, so "third" shows its nick.
            (should (string-match-p "<alice>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) third-bol)))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-group-messages-respects-time-window ()
  "Same-nick PRIVMSGs farther apart than the window are not grouped."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-group-messages-by-nick t)
        (clatter-group-messages-window 10)
        (t1 (seconds-to-time 1000))
        (t2 (seconds-to-time 1020)))
    (unwind-protect
        (clatter-test--with-grouping-buffer 'newest-first conn
          (clatter-insert-privmsg (current-buffer) "alice" "first" conn t1)
          (clatter-insert-privmsg (current-buffer) "alice" "second" conn t2)
          (let ((first-bol (clatter-test--find-line-bol (current-buffer) "first"))
                (second-bol (clatter-test--find-line-bol (current-buffer) "second")))
            ;; 20s gap exceeds the 10s window: both show the nick.
            (should (string-match-p "<alice>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) first-bol)))
            (should (string-match-p "<alice>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) second-bol)))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-group-messages-off-by-default ()
  "With grouping disabled, consecutive same-nick PRIVMSGs both show the nick."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-group-messages-by-nick nil))
    (unwind-protect
        (clatter-test--with-grouping-buffer 'newest-first conn
          (clatter-insert-privmsg (current-buffer) "alice" "first" conn)
          (clatter-insert-privmsg (current-buffer) "alice" "second" conn)
          (let ((first-bol (clatter-test--find-line-bol (current-buffer) "first"))
                (second-bol (clatter-test--find-line-bol (current-buffer) "second")))
            (should (string-match-p "<alice>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) first-bol)))
            (should (string-match-p "<alice>"
                                    (clatter-test--nick-column-at
                                     (current-buffer) second-bol)))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-group-messages-history-playback-uses-adjacency ()
  "History playback groups same-nick bursts by visual adjacency, ignoring the
time window, because backlog messages replay with widely spaced server-times."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (buf nil)
        (clatter-group-messages-by-nick t)
        (clatter-group-messages-window 10)
        ;; 30min apart: a live burst would never group these.
        (t1 (encode-time 0 0 11 1 1 2026 t))
        (t2 (encode-time 0 0 11 30 1 1 2026 t))
        (t3 (encode-time 0 0 12 1 1 2026 t)))
    (unwind-protect
        (progn
          (setq buf (clatter-get-or-create-buffer "testnet" "#chan" 'channel))
          (with-current-buffer buf (clatter-ui-setup-buffer buf))
          ;; Messages arrive oldest-first, as CHATHISTORY playback delivers them.
          (clatter-ui--on-batch-complete
           conn "chathistory" "#chan"
           (list (list :type 'privmsg :sender "alice" :text "first" :time t1)
                 (list :type 'privmsg :sender "alice" :text "second" :time t2)
                 (list :type 'privmsg :sender "alice" :text "third" :time t3)))
          (with-current-buffer buf
            (let ((first-bol (clatter-test--find-line-bol buf "first"))
                  (second-bol (clatter-test--find-line-bol buf "second"))
                  (third-bol (clatter-test--find-line-bol buf "third")))
              ;; First message of the burst keeps the nick; the rest blank it.
              (should (string-match-p "<alice>"
                                      (clatter-test--nick-column-at buf first-bol)))
              (should (string-match-p "\\` *\\'"
                                      (clatter-test--nick-column-at buf second-bol)))
              (should (string-match-p "\\` *\\'"
                                      (clatter-test--nick-column-at buf third-bol)))
              ;; The blanked columns still carry the sender for navigation.
              (should (equal (get-text-property second-bol 'clatter-sender) "alice"))
              (should (equal (get-text-property third-bol 'clatter-sender) "alice")))))
      (clatter-test-cleanup)
      (when buf
        (clatter-remove-buffer "testnet" "#chan")
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest clatter-group-messages-reads-previous-in-target-buffer ()
  "Grouping must read the previous line in the target buffer, not `current-buffer'.
The process filter runs with a small auxiliary buffer current while
`clatter-insert-privmsg' targets the channel buffer; reading the
previous message's text properties there used to raise
\"Args out of range\" because the position belonged to the channel."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-group-messages-by-nick t))
    (unwind-protect
        (clatter-test--with-grouping-buffer 'newest-first conn
          (let ((chan (current-buffer)))
            ;; Insert the first message normally to populate the buffer.
            (clatter-insert-privmsg chan "alice" "first" conn)
            ;; Insert the second from a tiny unrelated current buffer,
            ;; mimicking the process filter's calling context.
            (with-temp-buffer
              (insert "x")
              (clatter-insert-privmsg chan "alice" "second" conn))
            (let ((second-bol (clatter-test--find-line-bol chan "second")))
              (should second-bol)
              ;; No error, and the second same-nick line is blanked.
              (should (string-match-p "\\` *\\'"
                                      (clatter-test--nick-column-at
                                       chan second-bol)))
              (should (equal (get-text-property second-bol 'clatter-sender)
                             "alice")))))
      (remhash "testnet" clatter-connections))))

;;; Nick-column truncation

(ert-deftest clatter-nick-column-truncate-clips-long-nick ()
  "When truncation is on, a nick longer than the column is clipped to the width."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-nick-column-truncate t)
        (clatter-nick-column-width 10))
    (unwind-protect
        (clatter-test--with-grouping-buffer 'newest-first conn
          (clatter-insert-privmsg (current-buffer) "verylongnick" "hello" conn)
          (let ((bol (clatter-test--find-line-bol (current-buffer) "hello")))
            (should bol)
            (let ((line (save-excursion
                          (goto-char bol)
                          (buffer-substring-no-properties
                           bol (line-end-position)))))
              ;; The nick is clipped to the ellipsis form, sized so its
              ;; display width (counting `…' as two cells) equals the
              ;; column width: the column gets no leading pad, so the
              ;; closing `>' lines up with shorter nicks above/below it.
              (should (string-match-p "\\`<verylo…> hello" line))
              (should (= (clatter--nick-column-display-width "<verylo…>")
                         clatter-nick-column-width)))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-nick-column-truncate-off-overflows ()
  "With truncation off (the default), a long nick overflows the column."
  (let ((conn (clatter-test-make-connection "testnet" "me"))
        (clatter-nick-column-truncate nil)
        (clatter-nick-column-width 10))
    (unwind-protect
        (clatter-test--with-grouping-buffer 'newest-first conn
          (clatter-insert-privmsg (current-buffer) "verylongnick" "hello" conn)
          (let ((bol (clatter-test--find-line-bol (current-buffer) "hello")))
            (should bol)
            ;; The full nick is present and spills past the column width...
            (should (string-match-p
                     "<verylongnick>"
                     (save-excursion
                       (goto-char bol)
                       (buffer-substring-no-properties
                        bol (line-end-position)))))
            ;; ...so the char at the column boundary is still part of the nick.
            (should-not (= (char-after (+ bol clatter-nick-column-width))
                           ?\s))))
      (remhash "testnet" clatter-connections))))

(ert-deftest clatter-sender-format-controls-nick-prefix ()
  "`clatter-sender-format' shapes the sender prefix and its truncation."
  (should (equal (clatter--format-sender "alice") "<alice>"))
  (let ((clatter-sender-format "[%nick]"))
    (should (equal (clatter--format-sender "alice") "[alice]"))
    ;; delimiters survive truncation, ellipsis counts as two columns
    (should (equal (clatter--truncate-nick-column "[longnickname]" 8)
                   "[long…]")))
  (let ((clatter-sender-format "%nick:"))
    (should (equal (clatter--format-sender "alice") "alice:"))
    (should (equal (clatter--truncate-nick-column "longnickname:" 8)
                   "longn…:")))
  ;; notice form is unaffected
  (should (equal (clatter--truncate-nick-column "-longnickname-" 8)
                 "-long…-")))

(ert-deftest clatter-refresh-prompt-repeated-keeps-single-prompt ()
  "Repeated prompt refreshes replace the prompt instead of duplicating it.
With a bottom prompt the type-t markers must not drift past the inserted
prompt, or the next refresh appends a second prompt and messages land
below the input line."
  (let ((conn (clatter-test-make-connection "testnet" "trevhk"))
        (clatter-prompt-format "%n: "))
    (unwind-protect
        (dolist (order '(oldest-first newest-first))
          (with-temp-buffer
            (let ((clatter-message-order order))
              (clatter-mode)
              (setq-local clatter--network "testnet")
              (setq-local clatter--target "#test")
              (setq-local clatter--buffer-type 'channel)
              (clatter-ui-setup-buffer (current-buffer))
              ;; Two nick changes, each refreshing the prompt: the second
              ;; refresh is where drifted markers duplicated the prompt.
              (setf (clatter-connection-nick conn) "trev")
              (clatter--refresh-prompt)
              (setf (clatter-connection-nick conn) "trev2")
              (clatter--refresh-prompt)
              (let ((text (buffer-string)))
                (should-not (string-match-p "trevhk: " text))
                (should-not (string-match-p "trev: " text))
                (should (= 1 (with-temp-buffer
                               (insert text)
                               (count-matches "trev2: "
                                              (point-min) (point-max))))))
              ;; Messages must still land on the message side of the prompt.
              (clatter-insert-privmsg (current-buffer) "alice" "hello" conn)
              (let ((msg (save-excursion
                           (goto-char (point-min))
                           (search-forward "hello")
                           (point))))
                (if (eq order 'oldest-first)
                    (should (< msg (marker-position clatter--prompt-marker)))
                  (should (> msg (marker-position clatter--input-marker))))))))
      (remhash "testnet" clatter-connections))))

(provide 'test-ui)

;;; test-ui.el ends here
