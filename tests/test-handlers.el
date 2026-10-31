;;; test-handlers.el --- Tests for clatter-handlers.el dispatch -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-commands)

;; --- PRIVMSG dispatch ---

(ert-deftest clatter-test-dispatch-privmsg ()
  "PRIVMSG dispatches to clatter-privmsg-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PRIVMSG #emacs :hello everyone")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "hello everyone"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-privmsg-time-tag ()
  "PRIVMSG with @time tag dispatches to clatter-privmsg-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let* ((time "2011-10-19T16:40:51.620Z")
               (time-tag (concat "@time=" time))
               (calls (clatter-test-capture-hook
                       clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              (concat time-tag " :alice!~a@host PRIVMSG #emacs :hello everyone"))))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "hello everyone"))
            (should (equal (nth 4 args) (clatter-parse-iso8601 time)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-privmsg-ctcp-action ()
  "PRIVMSG CTCP ACTION dispatches to clatter-action-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PRIVMSG #emacs :ACTION greets everyone")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "greets everyone"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-privmsg-ctcp-action-time-tag ()
  "PRIVMSG CTCP ACTION with @time tag dispatches to clatter-action-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let* ((time "2011-10-19T16:40:51.620Z")
               (time-tag (concat "@time=" time))
               (calls (clatter-test-capture-hook
                       clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              (concat time-tag " :alice!~a@host PRIVMSG #emacs :ACTION greets everyone"))))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "greets everyone"))
            (should (equal (nth 4 args) (clatter-parse-iso8601 time)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-batch-playback-defers-privmsg ()
  "BATCH-tagged history bypasses live PRIVMSG hooks until completion."
  (let ((conn (clatter-test-make-connection))
        privmsg-calls
        batch-complete)
    (unwind-protect
        (let ((privmsg-handler (lambda (&rest args)
                                  (push args privmsg-calls)))
              (batch-handler (lambda (&rest args)
                               (setq batch-complete args))))
          (add-hook 'clatter-privmsg-hook privmsg-handler)
          (add-hook 'clatter-batch-complete-hook batch-handler)
          (unwind-protect
              (progn
                (clatter-dispatch-message
                 conn (clatter-test-parse
                       ":server BATCH +history chathistory #emacs"))
                (clatter-dispatch-message
                 conn (clatter-test-parse
                       "@batch=history :alice!~a@host PRIVMSG #emacs :old message"))
                (clatter-dispatch-message
                 conn (clatter-test-parse ":server BATCH -history"))
                (should-not privmsg-calls)
                (should (equal (nth 1 batch-complete) "chathistory"))
                (should (equal (nth 2 batch-complete) "#emacs"))
                (let ((message (car (nth 3 batch-complete))))
                  (should (eq (plist-get message :type) 'privmsg))
                  (should (equal (plist-get message :sender) "alice"))
                  (should (equal (plist-get message :text) "old message"))))
            (remove-hook 'clatter-privmsg-hook privmsg-handler)
            (remove-hook 'clatter-batch-complete-hook batch-handler)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-batch-playback-defers-action ()
  "BATCH-tagged /me history bypasses live ACTION hooks until completion."
  (let ((conn (clatter-test-make-connection))
        action-calls
        batch-complete)
    (unwind-protect
        (let ((action-handler (lambda (&rest args)
                                 (push args action-calls)))
              (batch-handler (lambda (&rest args)
                               (setq batch-complete args))))
          (add-hook 'clatter-action-hook action-handler)
          (add-hook 'clatter-batch-complete-hook batch-handler)
          (unwind-protect
              (progn
                (clatter-dispatch-message
                 conn (clatter-test-parse
                       ":server BATCH +history chathistory #emacs"))
                (clatter-dispatch-message
                 conn (clatter-test-parse
                       "@batch=history :alice!~a@host PRIVMSG #emacs :\C-aACTION waves\C-a"))
                (clatter-dispatch-message
                 conn (clatter-test-parse ":server BATCH -history"))
                (should-not action-calls)
                (let ((message (car (nth 3 batch-complete))))
                  (should (eq (plist-get message :type) 'action))
                  (should (equal (plist-get message :sender) "alice"))
                  (should (equal (plist-get message :text) "waves"))))
            (remove-hook 'clatter-action-hook action-handler)
            (remove-hook 'clatter-batch-complete-hook batch-handler)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-batch-playback-does-not-answer-ctcp ()
  "Replayed CTCP queries must neither be answered nor reach live hooks."
  (let ((conn (clatter-test-make-connection))
        (clatter-ctcp-hook (list (lambda (&rest _args)
                                   (ert-fail "Replayed CTCP reached a live hook")))))
    (unwind-protect
        (dolist (batch-type '("chathistory" "znc.in/playback"))
          (dolist (query '("PING ace80482-df03-49ff-aed2-c3d0d7f665b6"
                           "VERSION" "TIME" "FINGER"))
            (clatter-test-with-mock-send
             (let ((calls (clatter-test-capture-hook clatter-system-hook
                            (clatter-dispatch-message
                             conn (clatter-test-parse
                                   (format ":server BATCH +history %s knighthk"
                                           batch-type)))
                            (clatter-dispatch-message
                             conn (clatter-test-parse
                                   (format "@batch=history;time=2026-10-03T14:00:00Z :knighthk!~k@host PRIVMSG testnick :\C-a%s\C-a"
                                           query)))
                            (clatter-dispatch-message
                             conn (clatter-test-parse ":server BATCH -history")))))
               (should-not calls)
               (should-not clatter-test--sent-lines)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-live-ctcp-ping-is-answered ()
  "A live CTCP PING is still answered and announced."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (clatter-test-with-mock-send
         (let ((calls (clatter-test-capture-hook clatter-system-hook
                        (clatter-dispatch-message
                         conn (clatter-test-parse
                               ":knighthk!~k@host PRIVMSG testnick :\C-aPING ace80482-df03-49ff-aed2-c3d0d7f665b6\C-a")))))
           (should calls)
           (should (equal clatter-test--sent-lines
                          '("NOTICE knighthk :\C-aPING ace80482-df03-49ff-aed2-c3d0d7f665b6\C-a")))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-batch-playback-waits-for-cap-negotiation ()
  "Completed playback waits for CAP negotiation to finish."
  (let ((conn (clatter-test-make-connection))
        batch-complete
        sent
        registration-sent)
    (setf (clatter-connection-cap-negotiating conn) t)
    (unwind-protect
        (let ((batch-handler (lambda (&rest args)
                               (setq batch-complete args))))
          (add-hook 'clatter-batch-complete-hook batch-handler)
          (unwind-protect
              (cl-letf (((symbol-function 'clatter-send)
                         (lambda (_conn line)
                           (push line sent)))
                        ((symbol-function 'clatter-cap--send-registration)
                         (lambda (&rest _)
                           (setq registration-sent t))))
                (clatter-dispatch-message
                 conn (clatter-test-parse
                       ":server BATCH +history chathistory #emacs"))
                (clatter-dispatch-message
                 conn (clatter-test-parse
                       "@batch=history :alice!~a@host PRIVMSG #emacs :old message"))
                (clatter-dispatch-message
                 conn (clatter-test-parse ":server BATCH -history"))
                (should-not batch-complete)
                (clatter-cap--finish-negotiation conn)
                (should-not (clatter-connection-cap-negotiating conn))
                (should registration-sent)
                (should (equal (nreverse sent) '("CAP END")))
                (should (equal (nth 1 batch-complete) "chathistory"))
                (should (= (length batch-complete) 4))
                (should (eq (car batch-complete) conn))
                (should (equal (plist-get (car (nth 3 batch-complete)) :text)
                               "old message"))
                (setq batch-complete nil)
                (clatter--flush-deferred-batches conn)
                (should-not batch-complete))
            (remove-hook 'clatter-batch-complete-hook batch-handler)))
      (clatter-test-cleanup))))

;; --- Retained session resources ---

(defmacro clatter-test--with-batch-session (conn proc &rest body)
  "Run BODY with an owned production session CONN and pipe PROC."
  (declare (indent 2))
  `(let* ((clatter-connections (make-hash-table :test 'equal))
          (,conn (clatter-test-make-connection))
          (,proc (make-pipe-process :name "clatter-test-batch" :noquery t))
          (clatter-disconnect-hook nil)
          (clatter-privmsg-hook nil)
          (clatter-batch-complete-hook nil))
     (setf (clatter-connection-process ,conn) ,proc
           (clatter-connection-reconnect-enabled ,conn) nil)
     (process-put ,proc :clatter-network-id "testnet")
     (set-process-sentinel ,proc #'clatter--process-sentinel)
     (unwind-protect
         (cl-letf (((symbol-function 'clatter--watchdog) #'ignore)
                   ((symbol-function 'clatter--debug) #'ignore)
                   ((symbol-function 'clatter-send) #'ignore)
                   ((symbol-function 'clatter-cap--send-registration) #'ignore))
           ,@body)
       (when (process-live-p ,proc)
         (delete-process ,proc)))))

(defun clatter-test--batch-input (conn &rest lines)
  "Dispatch IRC LINES to CONN through the real parser and handlers."
  (dolist (line lines)
    (clatter-dispatch-message conn (clatter-test-parse line))))

(defun clatter-test--batch-resources-empty (conn)
  "Assert that CONN owns no retained batches or batch accounting."
  (should (zerop (clatter-connection-batch-bytes conn)))
  (should (zerop (clatter-connection-batch-message-count conn)))
  (should (zerop (clatter-connection-batch-count conn)))
  (should (zerop (hash-table-count (clatter-connection-active-batches conn))))
  (should-not (clatter-connection-deferred-batches conn)))

(ert-deftest clatter-test-batch-aggregate-byte-boundary ()
  "Active and CAP-deferred string bytes share one byte-exact ceiling."
  (dolist (overflow-p '(nil t))
    (clatter-test--with-batch-session conn proc
      ;; Each batch retains a one-byte ID/type and two-byte target.
      ;; Each message retains a one-byte sender and two-byte Unicode text.
      (let ((clatter-max-batch-bytes 14)
            calls reason
            (live-calls 0))
        (let ((clatter-disconnect-hook
               (list (lambda (_network why) (setq reason why))))
              (clatter-privmsg-hook
               (list (lambda (&rest _) (cl-incf live-calls))))
              (clatter-batch-complete-hook
               (list (lambda (&rest args) (push args calls)))))
          (setf (clatter-connection-cap-negotiating conn) t)
          (clatter-test--batch-input
           conn ":server BATCH +a h #x"
           "@batch=a :s!u@host PRIVMSG #x :é"
           ":server BATCH -a"
           ":server BATCH +b h #x")
          (should (= (clatter-connection-batch-bytes conn) 11))
          (clatter-test--batch-input
           conn (concat "@batch=b :s!u@host PRIVMSG #x :"
                        (if overflow-p "éx" "é")))
          (should-not calls)
          (should (zerop live-calls))
          (if overflow-p
              (progn
                (should (string-match-p "retained batch string bytes.*limit 14"
                                        reason))
                (should-not (process-live-p proc))
                (should-not (clatter-connection-process conn))
                (clatter-test--batch-resources-empty conn)
                (clatter--flush-deferred-batches conn)
                (should-not calls))
            (should (process-live-p proc))
            (should-not reason)
            (should (= (clatter-connection-batch-bytes conn) 14))
            (should (= (clatter-connection-batch-message-count conn) 2))
            (should (= (clatter-connection-batch-count conn) 2))
            (clatter-test--batch-input conn ":server BATCH -b")
            (should-not calls)
            (clatter-cap--finish-negotiation conn)
            (should (= (length calls) 2))
            (dolist (args calls)
              (should (= (length args) 4))
              (should (eq (car args) conn))
              (should (equal (nth 1 args) "h"))
              (should (equal (nth 2 args) "#x"))
              (should (equal (nth 3 args)
                             '((:type privmsg :sender "s" :text "é" :time nil)))))
            (clatter-test--batch-resources-empty conn)))))))

(ert-deftest clatter-test-batch-retained-message-properties-byte-boundary ()
  "Retained message IDs and reply references also consume string bytes."
  (dolist (overflow-p '(nil t))
    (clatter-test--with-batch-session conn proc
      (let ((clatter-max-batch-bytes 12)
            calls reason)
        (let ((clatter-disconnect-hook
               (list (lambda (_network why) (setq reason why))))
              (clatter-batch-complete-hook
               (list (lambda (&rest args) (push args calls)))))
          (clatter-test--batch-input
           conn ":server BATCH +a h #x"
           (format "@batch=a;msgid=%s;+draft/reply=rr :s!u@host PRIVMSG #x :é"
                   (if overflow-p "idx!" "idx")))
          (if overflow-p
              (progn
                (should (string-match-p "retained batch string bytes.*limit 12"
                                        reason))
                (should-not (process-live-p proc))
                (should-not calls)
                (clatter-test--batch-resources-empty conn))
            (should (= (clatter-connection-batch-bytes conn) 12))
            (clatter-test--batch-input conn ":server BATCH -a")
            (let ((text (plist-get (car (nth 3 (car calls))) :text)))
              (should (equal text "é"))
              (should (equal (get-text-property 0 'clatter-msgid text) "idx"))
              (should (equal (get-text-property 0 'clatter-reply-to text) "rr")))
            (clatter-test--batch-resources-empty conn)))))))

(ert-deftest clatter-test-batch-aggregate-message-boundary ()
  "CAP-deferred messages count toward the active batch's message ceiling."
  (dolist (overflow-p '(nil t))
    (clatter-test--with-batch-session conn proc
      (let ((clatter-max-batch-messages 2)
            calls reason)
        (let ((clatter-disconnect-hook
               (list (lambda (_network why) (setq reason why))))
              (clatter-batch-complete-hook
               (list (lambda (_conn _type _target messages)
                       (push (mapcar (lambda (message) (plist-get message :text))
                                     messages)
                             calls)))))
          (setf (clatter-connection-cap-negotiating conn) t)
          (clatter-test--batch-input
           conn ":server BATCH +a h #x"
           "@batch=a :s!u@host PRIVMSG #x :first"
           ":server BATCH -a"
           ":server BATCH +b h #x"
           "@batch=b :s!u@host PRIVMSG #x :second")
          (should (process-live-p proc))
          (should (= (clatter-connection-batch-message-count conn) 2))
          (should-not calls)
          (if overflow-p
              (progn
                (clatter-test--batch-input
                 conn "@batch=b :s!u@host PRIVMSG #x :third")
                (should (string-match-p "retained batch messages.*limit 2" reason))
                (should-not (process-live-p proc))
                (clatter-test--batch-resources-empty conn)
                (clatter--flush-deferred-batches conn)
                (should-not calls))
            (clatter-test--batch-input conn ":server BATCH -b")
            (clatter-cap--finish-negotiation conn)
            (should (equal (reverse calls) '(("first") ("second"))))
            (clatter-test--batch-resources-empty conn)))))))

(ert-deftest clatter-test-batch-aggregate-empty-count-boundary ()
  "Empty active and CAP-deferred batches still consume batch slots."
  (dolist (overflow-p '(nil t))
    (clatter-test--with-batch-session conn proc
      (let ((clatter-max-batches 2)
            calls reason)
        (let ((clatter-disconnect-hook
               (list (lambda (_network why) (setq reason why))))
              (clatter-batch-complete-hook
               (list (lambda (_conn _type target messages)
                       (should-not messages)
                       (push target calls)))))
          (setf (clatter-connection-cap-negotiating conn) t)
          (clatter-test--batch-input
           conn ":server BATCH +a h #first" ":server BATCH -a"
           ":server BATCH +b h #second")
          (should (process-live-p proc))
          (should (= (clatter-connection-batch-count conn) 2))
          (should (zerop (clatter-connection-batch-message-count conn)))
          (should-not calls)
          (if overflow-p
              (progn
                (clatter-test--batch-input conn ":server BATCH +c h #third")
                (should (string-match-p "retained batches.*limit 2" reason))
                (should-not (process-live-p proc))
                (clatter-test--batch-resources-empty conn)
                (clatter--flush-deferred-batches conn)
                (should-not calls))
            (clatter-test--batch-input conn ":server BATCH -b")
            (clatter-cap--finish-negotiation conn)
            (should (equal (reverse calls) '("#first" "#second")))
            (clatter-test--batch-resources-empty conn)))))))

(ert-deftest clatter-test-batch-empty-metadata-byte-boundary ()
  "Empty batches retain metadata bytes, not just message payload bytes."
  (dolist (overflow-p '(nil t))
    (clatter-test--with-batch-session conn proc
      (let ((clatter-max-batch-bytes 8)
            calls reason)
        (let ((clatter-disconnect-hook
               (list (lambda (_network why) (setq reason why))))
              (clatter-batch-complete-hook
               (list (lambda (&rest args) (push args calls)))))
          (setf (clatter-connection-cap-negotiating conn) t)
          (clatter-test--batch-input
           conn ":server BATCH +a h #x" ":server BATCH -a"
           (if overflow-p ":server BATCH +b h #xx" ":server BATCH +b h #x"))
          (if overflow-p
              (progn
                (should (string-match-p "retained batch string bytes.*limit 8"
                                        reason))
                (should-not (process-live-p proc))
                (clatter-test--batch-resources-empty conn)
                (clatter--flush-deferred-batches conn)
                (should-not calls))
            (should (process-live-p proc))
            (should (= (clatter-connection-batch-bytes conn) 8))
            (clatter-test--batch-input conn ":server BATCH -b")
            (clatter-cap--finish-negotiation conn)
            (should (= (length calls) 2))
            (should-not (nth 3 (car calls)))
            (clatter-test--batch-resources-empty conn)))))))

(ert-deftest clatter-test-batch-targets-aggregate-boundaries ()
  "TARGETS entries share the same byte and message ceilings as history."
  (dolist (resource '(bytes messages))
    (dolist (overflow-p '(nil t))
      (clatter-test--with-batch-session conn proc
        (let ((clatter-max-batch-bytes (if (eq resource 'bytes) 14 100))
              (clatter-max-batch-messages 2)
              calls reason)
          (let ((clatter-disconnect-hook
                 (list (lambda (_network why) (setq reason why))))
                (clatter-batch-complete-hook
                 (list (lambda (_conn _type _target messages)
                         (push messages calls)))))
            (setf (clatter-connection-cap-negotiating conn) t)
            (clatter-test--batch-input
             conn ":server BATCH +a h #x"
             "@batch=a :server CHATHISTORY TARGETS #é 2026-10-03T14:00:00Z"
             ":server BATCH -a"
             ":server BATCH +b h #x")
            (if (and overflow-p (eq resource 'bytes))
                ;; Eight metadata bytes plus targets of three and four bytes.
                (clatter-test--batch-input
                 conn "@batch=b :server CHATHISTORY TARGETS #éx")
              (clatter-test--batch-input
               conn "@batch=b :server CHATHISTORY TARGETS #é"))
            (when (and overflow-p (eq resource 'messages))
              (should (= (clatter-connection-batch-message-count conn) 2))
              (clatter-test--batch-input
               conn "@batch=b :server CHATHISTORY TARGETS #third"))
            (should-not calls)
            (if overflow-p
                (progn
                  (should (string-match-p
                           (if (eq resource 'bytes)
                               "retained batch string bytes.*limit 14"
                             "retained batch messages.*limit 2")
                           reason))
                  (should-not (process-live-p proc))
                  (clatter-test--batch-resources-empty conn)
                  (clatter--flush-deferred-batches conn)
                  (should-not calls))
              (should (process-live-p proc))
              (should (= (clatter-connection-batch-bytes conn) 14))
              (should (= (clatter-connection-batch-message-count conn) 2))
              (clatter-test--batch-input conn ":server BATCH -b")
              (clatter-cap--finish-negotiation conn)
              (setq calls (nreverse calls))
              (should (= (length calls) 2))
              (should (equal (car calls)
                             (list (list :type 'chathistory-targets :target "#é"
                                         :time (clatter-parse-iso8601
                                                "2026-10-03T14:00:00Z")))))
              (should (equal (cadr calls)
                             '((:type chathistory-targets :target "#é" :time nil))))
              (clatter-test--batch-resources-empty conn))))))))

(ert-deftest clatter-test-batch-completion-releases-before-error-and-reentry ()
  "Completion consumes ownership before an error or reuse of the same ID."
  (dolist (reenter-p '(nil t))
    (clatter-test--with-batch-session conn proc
      (let ((clatter-max-batch-bytes 7)
            (clatter-max-batch-messages 1)
            (clatter-max-batches 1)
            (calls 0))
        (let ((clatter-batch-complete-hook
               (list (lambda (received-conn type target messages)
                       (should (eq received-conn conn))
                       (should (equal type "h"))
                       (should (equal target "#x"))
                       (should (equal (plist-get (car messages) :text) "é"))
                       (clatter-test--batch-resources-empty conn)
                       (cl-incf calls)
                       (when (= calls 1)
                         (when reenter-p
                           (clatter-test--batch-input
                            conn ":server BATCH +a h #x"
                            "@batch=a :s!u@host PRIVMSG #x :é"))
                         (error "Completion hook failed"))))))
          (clatter-test--batch-input
           conn ":server BATCH +a h #x"
           "@batch=a :s!u@host PRIVMSG #x :é")
          (should-error (clatter-test--batch-input conn ":server BATCH -a"))
          (should (process-live-p proc))
          (should (= calls 1))
          (if reenter-p
              (progn
                (should (= (clatter-connection-batch-bytes conn) 7))
                (should (= (clatter-connection-batch-message-count conn) 1))
                (should (= (clatter-connection-batch-count conn) 1)))
            (clatter-test--batch-resources-empty conn)
            (clatter-test--batch-input conn ":server BATCH -a")
            (should (= calls 1))
            (clatter-test--batch-input
             conn ":server BATCH +a h #x"
             "@batch=a :s!u@host PRIVMSG #x :é"))
          (clatter-test--batch-input conn ":server BATCH -a")
          (should (= calls 2))
          (clatter-test--batch-resources-empty conn))))))

(ert-deftest clatter-test-batch-cap-flush-error-keeps-undelivered-order ()
  "A failing CAP hook consumes its batch, preserving older queued batches."
  (clatter-test--with-batch-session conn proc
    (let ((clatter-max-batch-bytes 21)
          (clatter-max-batch-messages 3)
          (clatter-max-batches 3)
          calls)
      (setf (clatter-connection-cap-negotiating conn) t)
      (dolist (id '("a" "b" "c"))
        (clatter-test--batch-input
         conn (format ":server BATCH +%s h #x" id)
         (format "@batch=%s :s!u@host PRIVMSG #x :%s%s" id id id)
         (format ":server BATCH -%s" id)))
      (let ((clatter-batch-complete-hook
             (list (lambda (_conn _type _target messages)
                     (let ((text (plist-get (car messages) :text)))
                       (push text calls)
                       (when (equal text "aa")
                         (should (= (clatter-connection-batch-bytes conn) 14))
                         (should (= (clatter-connection-batch-message-count conn) 2))
                         (should (= (clatter-connection-batch-count conn) 2))
                         (setf (clatter-connection-cap-negotiating conn) t)
                         (clatter-test--batch-input
                          conn ":server BATCH +d h #x"
                          "@batch=d :s!u@host PRIVMSG #x :dd"
                          ":server BATCH -d")
                         (setf (clatter-connection-cap-negotiating conn) nil)
                         (error "CAP delivery hook failed")))))))
        (should-error (clatter-cap--finish-negotiation conn))
        (should (process-live-p proc))
        (should (equal calls '("aa")))
        (should (= (clatter-connection-batch-bytes conn) 21))
        (should (= (clatter-connection-batch-message-count conn) 3))
        (should (= (clatter-connection-batch-count conn) 3))
        (clatter--flush-deferred-batches conn)
        (should (equal (reverse calls) '("aa" "bb" "cc" "dd")))
        (clatter-test--batch-resources-empty conn)
        (clatter--flush-deferred-batches conn)
        (should (= (length calls) 4))))))

(ert-deftest clatter-test-batch-disconnect-during-cap-flush ()
  "A disconnect inside delivery discards active and undelivered resources."
  (dolist (fail-p '(nil t))
    (clatter-test--with-batch-session conn proc
      (let (calls)
        (setf (clatter-connection-cap-negotiating conn) t)
        (clatter-test--batch-input
         conn ":server BATCH +a h #x"
         "@batch=a :s!u@host PRIVMSG #x :first"
         ":server BATCH -a"
         ":server BATCH +b h #x"
         "@batch=b :s!u@host PRIVMSG #x :second"
         ":server BATCH -b"
         ":server BATCH +c h #x"
         "@batch=c :s!u@host PRIVMSG #x :pending")
        (puthash "pending" #'ignore (clatter-connection-pending-labels conn))
        (let ((clatter-batch-complete-hook
               (list (lambda (_conn _type _target messages)
                       (push (plist-get (car messages) :text) calls)
                       (delete-process proc)
                       (when fail-p (error "Disconnected delivery failed"))))))
          (if fail-p
              (should-error (clatter-cap--finish-negotiation conn))
            (clatter-cap--finish-negotiation conn))
          (should (equal calls '("first")))
          (should-not (clatter-connection-process conn))
          (should (eq (clatter-connection-state conn) :disconnected))
          (clatter-test--batch-resources-empty conn)
          (should (zerop (hash-table-count
                          (clatter-connection-pending-labels conn))))
          (clatter--flush-deferred-batches conn)
          (should (equal calls '("first"))))))))

(ert-deftest clatter-test-label-error-consumes-callback ()
  "A failing labeled response callback is not retained or invoked twice."
  (clatter-test--with-batch-session conn proc
    (let ((calls 0)
          (message (clatter-test-parse "@label=reply :server NOTICE testnick :ok")))
      (puthash "reply"
               (lambda (received)
                 (should (eq received message))
                 (should-not (gethash "reply"
                                      (clatter-connection-pending-labels conn)))
                 (cl-incf calls)
                 (error "Label callback failed"))
               (clatter-connection-pending-labels conn))
      (should-error (clatter-dispatch-message conn message))
      (should (zerop (hash-table-count
                      (clatter-connection-pending-labels conn))))
      (clatter-dispatch-message conn message)
      (should (= calls 1))
      (should (process-live-p proc)))))

(ert-deftest clatter-test-label-reentrant-registration-survives-callback ()
  "Re-registering the same label in a callback survives return and error."
  (dolist (fail-p '(nil t))
    (clatter-test--with-batch-session conn proc
      (let ((old-calls 0)
            (new-calls 0)
            (message (clatter-test-parse "@label=reply :server NOTICE testnick :ok")))
        (let ((replacement (lambda (received)
                             (should (eq received message))
                             (cl-incf new-calls))))
          (puthash "reply"
                   (lambda (_received)
                     (cl-incf old-calls)
                     (puthash "reply" replacement
                              (clatter-connection-pending-labels conn))
                     (when fail-p (error "Old label callback failed")))
                   (clatter-connection-pending-labels conn))
          (if fail-p
              (should-error (clatter-dispatch-message conn message))
            (clatter-dispatch-message conn message))
          (should (eq (gethash "reply" (clatter-connection-pending-labels conn))
                      replacement))
          (clatter-dispatch-message conn message)
          (clatter-dispatch-message conn message)
          (should (= old-calls 1))
          (should (= new-calls 1))
          (should (zerop (hash-table-count
                          (clatter-connection-pending-labels conn))))
          (should (process-live-p proc)))))))

(ert-deftest clatter-test-dispatch-privmsg-dm ()
  "PRIVMSG to our nick dispatches with our nick as target."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":bob!~b@host PRIVMSG testnick :hey there")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("bob" "~b" "host")))
            (should (equal (nth 2 args) "testnick"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-privmsg-dm-ctcp-action ()
  "PRIVMSG CTCP ACTION to our nick dispatches with our nick as target."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":bob!~b@host PRIVMSG testnick :ACTION waves")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("bob" "~b" "host")))
            (should (equal (nth 2 args) "testnick"))))
      (clatter-test-cleanup))))

;; --- NOTICE dispatch ---

(ert-deftest clatter-test-batch-playback-ignores-ctcp-replies ()
  "Replayed CTCP replies are not printed as live system lines."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (dolist (batch-type '("chathistory" "znc.in/playback"))
          (dolist (reply '("PING ace80482-df03-49ff-aed2-c3d0d7f665b6"
                           "VERSION \C-bERC\C-b 5.6.2-git" "TIME old time"))
            (clatter-test-with-mock-send
             (let ((calls (clatter-test-capture-hook clatter-ctcp-reply-hook
                            (clatter-dispatch-message
                             conn (clatter-test-parse
                                   (format ":server BATCH +history %s knighthk"
                                           batch-type)))
                            (clatter-dispatch-message
                             conn (clatter-test-parse
                                   (format "@batch=history;time=2026-10-03T14:00:00Z :knighthk!~k@host NOTICE testnick :\C-a%s\C-a"
                                           reply)))
                            (clatter-dispatch-message
                             conn (clatter-test-parse ":server BATCH -history")))))
               (should-not calls)
               (should-not clatter-test--sent-lines)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-live-ctcp-ping-reply-is-shown ()
  "A live CTCP PING reply still reaches `clatter-ctcp-reply-hook'."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-ctcp-reply-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":knighthk!~k@host NOTICE testnick :\C-aPING ace80482-df03-49ff-aed2-c3d0d7f665b6\C-a")))))
          (should (= (length calls) 1))
          (should (equal (nth 2 (car calls)) "PING"))
          (should (equal (nth 3 (car calls)) "ace80482-df03-49ff-aed2-c3d0d7f665b6")))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-notice ()
  "NOTICE dispatches to clatter-notice-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-notice-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":NickServ!srv@services NOTICE testnick :You are identified")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("NickServ" "srv" "services")))
            (should (equal (nth 3 args) "You are identified"))))
      (clatter-test-cleanup))))

;; --- JOIN dispatch ---

(ert-deftest clatter-test-dispatch-join ()
  "JOIN dispatches to clatter-join-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-join-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host JOIN #emacs alice :Alice A")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "alice"))))
      (clatter-test-cleanup))))

;; --- PART dispatch ---

(ert-deftest clatter-test-dispatch-part ()
  "PART dispatches to clatter-part-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-part-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PART #emacs :bye")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "bye"))))
      (clatter-test-cleanup))))

;; --- QUIT dispatch ---

(ert-deftest clatter-test-dispatch-quit ()
  "QUIT dispatches to clatter-quit-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-quit-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host QUIT :connection reset")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "connection reset"))))
      (clatter-test-cleanup))))

;; --- NICK dispatch ---

(ert-deftest clatter-test-dispatch-nick ()
  "NICK dispatches to clatter-nick-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-nick-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host NICK :alice_")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "alice_"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-nick-self ()
  "NICK for self updates connection nick."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (progn
          (clatter-dispatch-message
           conn (clatter-test-parse
                 ":testnick!~t@host NICK :newnick"))
          (should (equal (clatter-connection-nick conn) "newnick")))
      (clatter-test-cleanup))))

;; --- TOPIC dispatch ---

(ert-deftest clatter-test-dispatch-topic ()
  "TOPIC dispatches to clatter-topic-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-topic-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host TOPIC #emacs :new topic here")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) "#emacs"))
            (should (equal (nth 2 args) '("alice" "~a" "host")))
            (should (equal (nth 3 args) "new topic here"))))
      (clatter-test-cleanup))))

;; --- PING/PONG ---

(ert-deftest clatter-test-dispatch-ping ()
  "PING sends PONG reply."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-dispatch-message
           conn (clatter-test-parse "PING :irc.libera.chat"))
          (should (clatter-test-sent-matching "PONG.*irc.libera.chat")))
      (clatter-test-cleanup))))

;; --- TAGMSG: Typing indicators ---

(ert-deftest clatter-test-dispatch-typing ()
  "TAGMSG with +typing dispatches to clatter-typing-hook."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-typing-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@+typing=active :alice!~a@host TAGMSG #emacs")))))
          (should (= (length calls) 1))
          (let ((args (car calls)))
            (should (equal (nth 1 args) '("alice" "~a" "host")))
            (should (equal (nth 2 args) "#emacs"))
            (should (equal (nth 3 args) "active"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-typing-self-ignored ()
  "Typing from self is not dispatched."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-typing-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@+typing=active :testnick!~t@host TAGMSG #emacs")))))
          (should (= (length calls) 0)))
      (clatter-test-cleanup))))

;; --- TAGMSG: Reactions ---

(ert-deftest clatter-test-dispatch-reaction ()
  "Standard and legacy reply tags preserve literal UTF-8 reaction values."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (dolist (reply-tag '("+reply" "+draft/reply" "draft/reply"))
          (let ((calls (clatter-test-capture-hook clatter-react-hook
                         (clatter-dispatch-message
                          conn (clatter-test-parse
                                (format "@+draft/react=👍;%s=abc123 :alice!~a@host TAGMSG #emacs"
                                        reply-tag))))))
            (should (= (length calls) 1))
            (let ((args (car calls)))
              (should (equal (nth 1 args) '("alice" "~a" "host")))
              (should (equal (nth 3 args) "👍"))
              (should (equal (nth 4 args) "abc123")))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-batch-reaction-byte-boundary-and-cap-deferral ()
  "Reaction string fields count against the shared ceiling through CAP deferral."
  (dolist (overflow-p '(nil t))
    (clatter-test--with-batch-session conn proc
      ;; Batch: a + h + #x = 4 bytes; reaction: é + 👍 + parent + #x = 14.
      (let ((clatter-max-batch-bytes 18)
            (clatter-max-batch-messages 1)
            (live-calls 0)
            delivered reason)
        (let ((clatter-react-hook (list (lambda (&rest _) (cl-incf live-calls))))
              (clatter-batch-complete-hook
               (list (lambda (&rest args) (setq delivered args))))
              (clatter-disconnect-hook
               (list (lambda (_network why) (setq reason why)))))
          (setf (clatter-connection-cap-negotiating conn) t)
          (clatter-test--batch-input
           conn ":server BATCH +a h #x"
           (format "@batch=a;+draft/react=%s;+reply=parent :é!longuser@longhost TAGMSG #x"
                   (if overflow-p "👍x" "👍")))
          (should (zerop live-calls))
          (should-not delivered)
          (if overflow-p
              (progn
                (should (string-match-p "retained batch string bytes.*limit 18" reason))
                (should-not (process-live-p proc))
                (clatter-test--batch-resources-empty conn))
            (should (= 18 (clatter-connection-batch-bytes conn)))
            (should (= 1 (clatter-connection-batch-message-count conn)))
            (clatter-test--batch-input conn ":server BATCH -a")
            (should-not delivered)
            (should (= 18 (clatter-connection-batch-bytes conn)))
            (clatter-cap--finish-negotiation conn)
            (should (equal (nth 3 delivered)
                           '((:type react :sender "é" :emoji "👍"
                              :parent-msgid "parent" :target "#x" :time nil))))
            (clatter-test--batch-resources-empty conn)))))))

(ert-deftest clatter-test-batch-reaction-message-ceiling ()
  "A reaction-only batch cannot bypass the retained event count ceiling."
  (clatter-test--with-batch-session conn proc
    (let ((clatter-max-batch-messages 1)
          reason)
      (let ((clatter-disconnect-hook
             (list (lambda (_network why) (setq reason why)))))
        (clatter-test--batch-input
         conn ":server BATCH +a chathistory #x"
         "@batch=a;+draft/react=👍;+reply=p :alice!u@host TAGMSG #x")
        (should (= 1 (clatter-connection-batch-message-count conn)))
        (clatter-test--batch-input
         conn "@batch=a;+draft/react=❤;+reply=p :bob!u@host TAGMSG #x")
        (should (string-match-p "retained batch messages.*limit 1" reason))
        (should-not (process-live-p proc))
        (clatter-test--batch-resources-empty conn)))))

;; --- BOT tag ---

(ert-deftest clatter-test-dispatch-bot-tag ()
  "PRIVMSG with bot tag marks sender."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@bot :botuser!~b@host PRIVMSG #emacs :automated msg")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (sender (clatter-prefix-nick (nth 1 args))))
            (should (get-text-property 0 'clatter-bot sender))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-bot-tag-ctcp-action ()
  "PRIVMSG CTCP ACTION with bot tag marks sender."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@bot :botuser!~b@host PRIVMSG #emacs :ACTION automated msg")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (sender (clatter-prefix-nick (nth 1 args))))
            (should (get-text-property 0 'clatter-bot sender))))
      (clatter-test-cleanup))))

;; --- Reply/Thread tags ---

(ert-deftest clatter-test-dispatch-reply-tag ()
  "PRIVMSG with +reply tag attaches reply-to property."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@+reply=msg999;msgid=msg1000 :alice!~a@host PRIVMSG #emacs :replying")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (text (nth 3 args)))
            (should (equal (get-text-property 0 'clatter-reply-to text) "msg999"))
            (should (equal (get-text-property 0 'clatter-msgid text) "msg1000"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-reply-tag-ctcp-action ()
  "PRIVMSG CTCP ACTION with draft/reply tag attaches reply-to property."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              "@+draft/reply=msg999;msgid=msg1000 :alice!~a@host PRIVMSG #emacs :ACTION replying")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (text (nth 3 args)))
            (should (equal (get-text-property 0 'clatter-reply-to text) "msg999"))
            (should (equal (get-text-property 0 'clatter-msgid text) "msg1000"))))
      (clatter-test-cleanup))))

;; --- RENAME dispatch ---

(ert-deftest clatter-test-dispatch-rename ()
  "RENAME dispatches system message (no crash even without buffer)."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-system-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":server RENAME #old #new :channel migrated")))))
          ;; No buffer for #old exists, so no system hook fires for rename
          ;; but the dispatch should not error
          (should t))
      (clatter-test-cleanup))))

;; --- MARKREAD dispatch ---

(ert-deftest clatter-test-dispatch-markread ()
  "MARKREAD is dispatched without error."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (progn
          (clatter-dispatch-message
           conn (clatter-test-parse
                 ":server MARKREAD #emacs timestamp=2026-01-01T00:00:00.000Z"))
          (should t))
      (clatter-test-cleanup))))

;; --- STATUSMSG ---

(ert-deftest clatter-test-dispatch-statusmsg ()
  "PRIVMSG to @#channel strips prefix and adds label."
  (let ((conn (clatter-test-make-connection)))
    ;; Set up ISUPPORT with STATUSMSG
    (let ((isup (make-hash-table :test 'equal)))
      (puthash "STATUSMSG" "@+" isup)
      (setf (clatter-connection-isupport conn) isup))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-privmsg-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PRIVMSG @#emacs :ops only message")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (target (nth 2 args))
                 (text (nth 3 args)))
            ;; Target should be #emacs (prefix stripped)
            (should (equal target "#emacs"))
            ;; Text should contain [ops] prefix
            (should (string-match-p "\\[ops\\]" text))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-statusmsg-ctcp-action ()
  "PRIVMSG CTCP ACTION to @#channel strips prefix and adds label."
  (let ((conn (clatter-test-make-connection)))
    ;; Set up ISUPPORT with STATUSMSG
    (let ((isup (make-hash-table :test 'equal)))
      (puthash "STATUSMSG" "@+" isup)
      (setf (clatter-connection-isupport conn) isup))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-action-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":alice!~a@host PRIVMSG @#emacs :ACTION ops only action")))))
          (should (= (length calls) 1))
          (let* ((args (car calls))
                 (target (nth 2 args))
                 (text (nth 3 args)))
            ;; Target should be #emacs (prefix stripped)
            (should (equal target "#emacs"))
            ;; Text should contain [ops] prefix
            (should (string-match-p "\\[ops\\]" text))))
      (clatter-test-cleanup))))

;; --- MONITOR numerics ---

(ert-deftest clatter-test-dispatch-monitor-online ()
  "730 RPL_MONONLINE dispatches system message."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-system-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":server 730 testnick :alice!~a@host")))))
          (should (= (length calls) 1))
          (should (string-match-p "Online" (nth 1 (car calls)))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-monitor-offline ()
  "731 RPL_MONOFFLINE dispatches system message."
  (let ((conn (clatter-test-make-connection)))
    (unwind-protect
        (let ((calls (clatter-test-capture-hook clatter-system-hook
                       (clatter-dispatch-message
                        conn (clatter-test-parse
                              ":server 731 testnick :alice")))))
          (should (= (length calls) 1))
          (should (string-match-p "Offline" (nth 1 (car calls)))))
      (clatter-test-cleanup))))

;; --- Nick in use (433) ---

;; --- Bouncer authentication and nick reclaim ---

(defmacro clatter-test--with-live-config (conn config &rest body)
  "Run BODY with CONN's process configured with CONFIG."
  (declare (indent 2))
  `(let ((proc (make-pipe-process :name "clatter-test-live-config" :buffer nil)))
     (unwind-protect
         (progn
           (setf (clatter-connection-process ,conn) proc)
           (process-put proc :clatter-config ,config)
           ;; The welcome path records watchdog diagnostics; tests do not
           ;; need or have permission to write the user state directory.
           (cl-letf (((symbol-function 'clatter--watchdog)
                      (lambda (&rest _))))
             ,@body))
       (when (process-live-p proc)
         (delete-process proc)))))

(ert-deftest clatter-test-welcome-bouncer-skips-nickserv-identify ()
  "A bouncer password is not sent to NickServ after 001."
  (let ((conn (clatter-test-make-connection "znc" "testnick")))
    (setf (clatter-connection-sasl-state conn) nil)
    (unwind-protect
        (clatter-test--with-live-config
            conn '(:nick "testnick" :password "bouncer-secret" :bouncer t)
          (clatter-test-with-mock-send
            (clatter-dispatch-message
             conn (clatter-test-parse ":server 001 testnick :Welcome"))
            (should-not (clatter-test-sent-matching "NickServ"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-welcome-identifies-for-non-bouncer-live-config ()
  "Non-bouncer connections retain automatic IDENTIFY using live config."
  (let ((conn (clatter-test-make-connection "znc" "testnick")))
    (setf (clatter-connection-sasl-state conn) nil)
    (unwind-protect
        (clatter-test--with-live-config
            conn '(:nick "testnick" :password "live-secret")
          (clatter-test-with-mock-send
            (clatter-dispatch-message
             conn (clatter-test-parse ":server 001 testnick :Welcome"))
            (should (clatter-test-sent-matching
                     "PRIVMSG NickServ :IDENTIFY live-secret"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-welcome-auto-identify-can-be-disabled ()
  "`clatter-auto-identify' suppresses IDENTIFY for direct connections."
  (let ((conn (clatter-test-make-connection "testnet" "testnick"))
        (clatter-auto-identify nil))
    (setf (clatter-connection-sasl-state conn) nil)
    (unwind-protect
        (clatter-test--with-live-config
            conn '(:nick "testnick" :password "server-secret")
          (clatter-test-with-mock-send
            (clatter-dispatch-message
             conn (clatter-test-parse ":server 001 testnick :Welcome"))
            (should-not (clatter-test-sent-matching "NickServ"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-welcome-sasl-still-suppresses-identify ()
  "SASL success continues to suppress automatic NickServ IDENTIFY."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (clatter-test--with-live-config
            conn '(:nick "testnick" :password "server-secret")
          (clatter-test-with-mock-send
            (clatter-dispatch-message
             conn (clatter-test-parse ":server 001 testnick :Welcome"))
            (should-not (clatter-test-sent-matching "NickServ"))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-bouncer-disables-automatic-nick-reclaim ()
  "Bouncer connections never start automatic NICK or REGAIN reclaim."
  (let ((conn (clatter-test-make-connection "znc" "fallback")))
    (setf (clatter-connection-desired-nick conn) "wanted"
          (clatter-connection-sasl-state conn) :done)
    (unwind-protect
        (clatter-test--with-live-config conn '(:bouncer t)
          (clatter-test-with-mock-send
            (let ((clatter-nick-reclaim-use-regain t))
              (clatter--maybe-start-nick-reclaim conn)
              (clatter--reclaim-nick conn "wanted"))
            (should-not (clatter-connection-nick-reclaim-timer conn))
            (should-not clatter-test--sent-lines)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-reconnect-preserves-bouncer-override ()
  "Reconnect retries with original overrides, including `:bouncer'."
  (let ((conn (clatter-test-make-connection "znc" "testnick"))
        scheduled reconnect-args)
    (setf (clatter-connection-state conn) :disconnected
          (clatter-connection-config conn)
          '(:server "znc.example" :nick "testnick" :bouncer t)
          (clatter-connection-connect-overrides conn) '(:bouncer t))
    (unwind-protect
        (cl-letf (((symbol-function 'clatter--watchdog) (lambda (&rest _)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args)
                     (setq scheduled (car (last args)))
                     'clatter-test-timer))
                  ((symbol-function 'clatter-connect)
                   (lambda (&rest args) (setq reconnect-args args))))
          (clatter--schedule-reconnect conn)
          (funcall scheduled)
          (should (equal reconnect-args '("znc" :bouncer t))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-reconnect-schedules-only-one-pending-timer ()
  "Repeated disconnect handling cannot accumulate reconnect timers."
  (let ((conn (clatter-test-make-connection "retry" "testnick"))
        (scheduled 0))
    (setf (clatter-connection-state conn) :disconnected)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _)
                     (cl-incf scheduled)
                     'clatter-test-reconnect-timer)))
          (clatter--schedule-reconnect conn)
          (clatter--schedule-reconnect conn)
          (should (= scheduled 1))
          (should (= (clatter-connection-reconnect-attempts conn) 1))
          (should (eq (clatter-connection-reconnect-timer conn)
                      'clatter-test-reconnect-timer)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-reconnect-gives-up-after-max-attempts ()
  "Once attempts reach `clatter-reconnect-max-attempts', reconnect gives up.
A persistent, user-actionable failure (unavailable SASL credentials, a
SASL-required server) must stop looping instead of retrying forever.  The
attempt count is left as-is and auto-reconnect is disabled."
  (let ((conn (clatter-test-make-connection "retry" "testnick"))
        (clatter-reconnect-max-attempts 2)
        (scheduled 0))
    (setf (clatter-connection-state conn) :disconnected
          (clatter-connection-reconnect-attempts conn) 2)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _)
                     (cl-incf scheduled)
                     'clatter-test-reconnect-timer))
                  ((symbol-function 'clatter--watchdog) (lambda (&rest _))))
          (clatter--schedule-reconnect conn)
          (should-not (clatter-connection-reconnect-enabled conn))
          (should (= scheduled 0))
          (should (= (clatter-connection-reconnect-attempts conn) 2))
          (should-not (clatter-connection-reconnect-timer conn)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-connect-resets-exhausted-reconnect-cycle ()
  "An explicit `clatter-connect' after a give-up starts a fresh retry cycle.
Previously the attempt count was left at the cap, so the first failure of
the new connection gave up immediately and every manual connect was a
single shot (the reported \"no reconnect after the 10th try\" bug)."
  (let* ((conn (clatter-test-make-connection "revive" "testnick"))
         (clatter-reconnect-max-attempts 2)
         (stored (list :server "irc.example" :nick "testnick" :tls nil))
         (clatter-networks (list (cons "revive" stored)))
         (procs nil))
    (setf (clatter-connection-state conn) :disconnected
          (clatter-connection-reconnect-enabled conn) nil
          (clatter-connection-reconnect-disabled-reason conn) :max-attempts
          (clatter-connection-reconnect-attempts conn) 2)
    (unwind-protect
        (cl-letf (((symbol-function 'make-network-process)
                   (lambda (&rest _)
                     (let ((p (make-pipe-process :name "clatter-test-revive" :noquery t)))
                       (push p procs) p)))
                  ((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
                  ((symbol-function 'clatter--watchdog) (lambda (&rest _))))
          (should (clatter-connect "revive"))
          (should (clatter-connection-reconnect-enabled conn))
          (should (eq (clatter-connection-reconnect-disabled-reason conn) nil))
          (should (zerop (clatter-connection-reconnect-attempts conn)))
          ;; Simulate the new attempt failing (sentinel sets :disconnected).
          (setf (clatter-connection-state conn) :disconnected)
          (clatter--schedule-reconnect conn)
          (should (clatter-connection-reconnect-enabled conn))
          (should (eq (clatter-connection-reconnect-timer conn) 'fake-timer))
          (should (= (clatter-connection-reconnect-attempts conn) 1)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-auto-retry-continues-backoff-cycle ()
  "A timer-driven retry keeps the current backoff cycle.
If the auto-retry reset the attempt count, every attempt would restart
the cycle at the initial delay and the give-up cap would never be
reached (infinite 10s retry loop)."
  (let* ((conn (clatter-test-make-connection "cycle" "testnick"))
         (clatter-reconnect-max-attempts 3)
         (stored (list :server "irc.example" :nick "testnick" :tls nil))
         (clatter-networks (list (cons "cycle" stored)))
         (callback nil))
    (setf (clatter-connection-state conn) :disconnected
          (clatter-connection-reconnect-attempts conn) 2
          (clatter-connection-connect-overrides conn) nil)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat function)
                     (setq callback function)
                     'clatter-test-reconnect-timer))
                  ((symbol-function 'make-network-process)
                   (lambda (&rest _)
                     (make-pipe-process :name "clatter-test-cycle" :noquery t)))
                  ((symbol-function 'clatter--watchdog) (lambda (&rest _)))
                  ((symbol-function 'message) (lambda (&rest _))))
          (clatter--schedule-reconnect conn)
          (should (eq (clatter-connection-reconnect-attempts conn) 3))
          (funcall callback)
          ;; The auto-retry must not have reset the cycle count.
          (should (= (clatter-connection-reconnect-attempts conn) 3))
          ;; And the next failure gives up, proving the cap still advances.
          (setf (clatter-connection-state conn) :disconnected)
          (clatter--schedule-reconnect conn)
          (should-not (clatter-connection-reconnect-enabled conn)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-reconnect-max-attempts-nil-never-gives-up ()
  "A nil `clatter-reconnect-max-attempts' preserves the old unlimited behavior."
  (let ((conn (clatter-test-make-connection "retry" "testnick"))
        (clatter-reconnect-max-attempts nil)
        (scheduled 0))
    (setf (clatter-connection-state conn) :disconnected
          (clatter-connection-reconnect-attempts conn) 1000)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (&rest _)
                     (cl-incf scheduled)
                     'clatter-test-reconnect-timer))
                  ((symbol-function 'clatter--watchdog) (lambda (&rest _))))
          (clatter--schedule-reconnect conn)
          (should (clatter-connection-reconnect-enabled conn))
          (should (= scheduled 1))
          (should (= (clatter-connection-reconnect-attempts conn) 1001)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-stale-reconnect-callback-cannot-own-newer-timer ()
  "An obsolete reconnect callback cannot clear or run a replacement timer."
  (let ((conn (clatter-test-make-connection "retry" "testnick"))
        callback
        connect-called)
    (setf (clatter-connection-state conn) :disconnected)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat function)
                     (setq callback function)
                     'old-timer))
                  ((symbol-function 'clatter-connect)
                   (lambda (&rest _)
                     (setq connect-called t))))
          (clatter--schedule-reconnect conn)
          (setf (clatter-connection-reconnect-timer conn) 'new-timer)
          (funcall callback)
          (should-not connect-called)
          (should (eq (clatter-connection-reconnect-timer conn) 'new-timer)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-sentinel-ignores-superseded-process ()
  "A late sentinel from an old process cannot disconnect its replacement."
  (let* ((conn (clatter-test-make-connection "retry" "testnick"))
         (old-process 'old-process)
         (new-process 'new-process)
         scheduled)
    (setf (clatter-connection-process conn) new-process
          (clatter-connection-state conn) :connected)
    (unwind-protect
        (cl-letf (((symbol-function 'process-get)
                   (lambda (process property)
                     (and (eq process old-process)
                          (eq property :clatter-network-id)
                          "retry")))
                  ((symbol-function 'clatter--schedule-reconnect)
                   (lambda (_conn) (setq scheduled t))))
          (clatter--process-sentinel old-process "deleted\n")
          (should-not scheduled)
          (should (eq (clatter-connection-process conn) new-process))
          (should (eq (clatter-connection-state conn) :connected)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-sentinel-deletes-dead-process ()
  "The sentinel removes the dead process from `process-list'.
Without this, every failed or retried clatter connection lingers in
`M-x list-processes' until Emacs is restarted."
  (let* ((conn (clatter-test-make-connection "dead" "testnick"))
         (proc (make-pipe-process :name "clatter-test-dead"
                                 :buffer nil :noquery t))
         (tls-buf (get-buffer-create " *clatter-tls-dead*")))
    (setf (clatter-connection-process conn) proc
          (clatter-connection-state conn) :connected
          (clatter-connection-reconnect-enabled conn) nil)
    (process-put proc :clatter-network-id "dead")
    (unwind-protect
        (progn
          (should (memq proc (process-list)))
          (should (buffer-live-p tls-buf))
          (clatter--process-sentinel proc "connection broken by remote peer\n")
          (should-not (memq proc (process-list)))
          (should-not (buffer-live-p tls-buf))
          (should-not (clatter-connection-process conn))
          (should (eq (clatter-connection-state conn) :disconnected)))
      (when (process-live-p proc)
        (delete-process proc))
      (when (buffer-live-p tls-buf)
        (kill-buffer tls-buf))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-reconnect-stable-timer-belongs-to-current-process ()
  "Only the current process's stable timer can reset reconnect backoff."
  (let* ((conn (clatter-test-make-connection "retry" "testnick"))
         (first-process (make-pipe-process :name "clatter-test-stable-1" :noquery t))
         (second-process (make-pipe-process :name "clatter-test-stable-2" :noquery t))
         callback
         cancelled)
    (setf (clatter-connection-process conn) first-process
          (clatter-connection-reconnect-attempts conn) 4
          (clatter-connection-regain-kill-count conn) 2
          (clatter-connection-regain-kill-time conn) 10.0)
    (unwind-protect
        (cl-letf (((symbol-function 'run-at-time)
                   (lambda (_delay _repeat function)
                     (setq callback function)
                     'stable-timer))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (setq cancelled timer))))
          (clatter--start-reconnect-stable-timer conn)
          (setf (clatter-connection-process conn) second-process)
          (funcall callback)
          (should (= (clatter-connection-reconnect-attempts conn) 4))
          (setf (clatter-connection-reconnect-stable-timer conn) 'old-stable)
          (clatter--start-reconnect-stable-timer conn)
          (should (eq cancelled 'old-stable)))
      (ignore-errors (delete-process first-process))
      (ignore-errors (delete-process second-process))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-nickserv-recovery-never-sends-network-password ()
  "Manual GHOST and REGAIN never append a server or bouncer password."
  (let ((conn (clatter-test-make-connection "znc" "testnick"))
        (clatter-networks '(("znc" :password "bouncer-secret"))))
    (unwind-protect
        (cl-letf (((symbol-function 'clatter-insert-system) (lambda (&rest _))))
          (clatter-test-with-mock-send
            (clatter--nickserv-recover conn "GHOST" "wanted")
            (clatter--nickserv-recover conn "REGAIN" "wanted")
            (should (equal (nreverse clatter-test--sent-lines)
                           '("PRIVMSG NickServ :GHOST wanted"
                             "PRIVMSG NickServ :REGAIN wanted")))))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-nick-in-use ()
  "433 during registration appends underscore to nick and retries."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (setf (clatter-connection-state conn) :connecting)
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-dispatch-message
           conn (clatter-test-parse
                 ":server 433 * testnick :Nickname is already in use"))
          (should (equal (clatter-connection-nick conn) "testnick_"))
          (should (clatter-test-sent-matching "NICK testnick_")))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-nick-in-use-while-connected ()
  "433 while connected (reclaim attempt) does not append underscore."
  (let ((conn (clatter-test-make-connection "testnet" "testnick_")))
    (setf (clatter-connection-desired-nick conn) "testnick")
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-dispatch-message
           conn (clatter-test-parse
                 ":server 433 * testnick :Nickname is already in use"))
          (should (equal (clatter-connection-nick conn) "testnick_"))
          (should-not (clatter-test-sent-matching "NICK")))
      (clatter-test-cleanup))))

;; --- Disconnect messages ---

(ert-deftest clatter-test-classify-disconnect-event ()
  "Known sentinel events are mapped to human-readable text; others pass through."
  (should (equal (clatter--classify-disconnect-event
                  "connection broken by remote peer\n")
                 "Connection closed by remote peer"))
  (should (equal (clatter--classify-disconnect-event "connection broken by peer\n")
                 "Connection closed by peer"))
  (should (equal (clatter--classify-disconnect-event "finished\n")
                 "Connection process exited"))
  (should (equal (clatter--classify-disconnect-event "deleted process\n")
                 "Connection closed"))
  ;; Unrecognized events fall through, trimmed but otherwise unchanged.
  (should (equal (clatter--classify-disconnect-event "open\n") "open"))

(ert-deftest clatter-test-disconnect-message-prefers-reason-slot ()
  "`clatter--disconnect-message' prefers a caller-set reason and clears the slot."
  (let ((conn (clatter-test-make-connection "n" "nick")))
    (unwind-protect
        (progn
          (setf (clatter-connection-disconnect-reason conn) "Custom reason")
          (should (equal (clatter--disconnect-message conn "finished\n")
                         "Custom reason"))
          ;; The slot is consumed, so the next call falls back to classification.
          (should-not (clatter-connection-disconnect-reason conn))
          (should (equal (clatter--disconnect-message conn "finished\n")
                         "Connection process exited")))
      (clatter-test-cleanup))))

;; --- Reconnect disable reasons ---

(ert-deftest clatter-test-explicit-disconnect-records-user-reason ()
  "An explicit disconnect disables reconnect with a `:user' reason."
  (let ((conn (clatter-test-make-connection "n" "nick")))
    (unwind-protect
        (cl-letf (((symbol-function 'message) (lambda (&rest _))))
          (clatter-disconnect "n")
          (should-not (clatter-connection-reconnect-enabled conn))
          (should (eq (clatter-connection-reconnect-disabled-reason conn) :user))
          (should (eq (clatter-connection-state conn) :disconnected)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-dispatch-banned-records-banned-reason ()
  "465 disables auto-reconnect with a `:banned' reason."
  (let ((conn (clatter-test-make-connection "testnet" "testnick")))
    (unwind-protect
        (clatter-test-with-mock-send
          (clatter-dispatch-message
           conn (clatter-test-parse ":server 465 * :You are banned from this server"))
          (should-not (clatter-connection-reconnect-enabled conn))
          (should (eq (clatter-connection-reconnect-disabled-reason conn) :banned)))
      (clatter-test-cleanup))))

;; --- Suspend / resume re-establishment ---

(ert-deftest clatter-test-resume-kills-half-open-and-resets-backoff ()
  "On resume a live (half-open) process is killed and backoff is reset."
  (let ((conn (clatter-test-make-connection "n" "nick"))
        (proc (make-pipe-process :name "clatter-test-resume-live" :buffer nil)))
    (setf (clatter-connection-process conn) proc)
    (setf (clatter-connection-state conn) :connected)
    (setf (clatter-connection-reconnect-attempts conn) 5)
    (unwind-protect
        (cl-letf (((symbol-function 'clatter--watchdog) (lambda (&rest _))))
          (clatter--reschedule-after-resume conn)
          (should-not (process-live-p proc))
          (should (= (clatter-connection-reconnect-attempts conn) 0)))
      (when (process-live-p proc) (delete-process proc))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-resume-replaces-pending-backoff ()
  "On resume a pending backoff timer is replaced by a fresh schedule."
  (let ((conn (clatter-test-make-connection "n" "nick"))
        (scheduled nil))
    (setf (clatter-connection-state conn) :disconnected)
    (setf (clatter-connection-reconnect-attempts conn) 7)
    (setf (clatter-connection-reconnect-timer conn) 'clatter-test-stale-timer)
    (unwind-protect
        (cl-letf (((symbol-function 'clatter--watchdog) (lambda (&rest _)))
                  ((symbol-function 'cancel-timer) (lambda (_)))
                  ((symbol-function 'message) (lambda (&rest _)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args)
                     (setq scheduled (car (last args)))
                     'clatter-test-timer)))
          (clatter--reschedule-after-resume conn)
          (should (eq (clatter-connection-reconnect-timer conn) 'clatter-test-timer))
          (should (= (clatter-connection-reconnect-attempts conn) 1))
          (should (functionp scheduled)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-resume-revives-transient-give-up ()
  "A max-attempts give-up is revived on resume and rescheduled."
  (let ((conn (clatter-test-make-connection "n" "nick"))
        (scheduled nil))
    (setf (clatter-connection-state conn) :disconnected)
    (setf (clatter-connection-reconnect-enabled conn) nil)
    (setf (clatter-connection-reconnect-disabled-reason conn) :max-attempts)
    (setf (clatter-connection-reconnect-attempts conn) 10)
    (unwind-protect
        (cl-letf (((symbol-function 'clatter--watchdog) (lambda (&rest _)))
                  ((symbol-function 'message) (lambda (&rest _)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest args)
                     (setq scheduled (car (last args)))
                     'clatter-test-timer)))
          (clatter--reschedule-after-resume conn)
          (should (clatter-connection-reconnect-enabled conn))
          (should-not (clatter-connection-reconnect-disabled-reason conn))
          (should (eq (clatter-connection-reconnect-timer conn) 'clatter-test-timer))
          (should (functionp scheduled)))
      (clatter-test-cleanup))))

(ert-deftest clatter-test-resume-does-not-revive-user-disconnect ()
  "An explicit user disconnect is left down across a resume."
  (let ((conn (clatter-test-make-connection "n" "nick"))
        (scheduled nil))
    (setf (clatter-connection-state conn) :disconnected)
    (setf (clatter-connection-reconnect-enabled conn) nil)
    (setf (clatter-connection-reconnect-disabled-reason conn) :user)
    (unwind-protect
        (cl-letf (((symbol-function 'clatter--watchdog) (lambda (&rest _)))
                  ((symbol-function 'run-at-time)
                   (lambda (&rest _) (setq scheduled t) 'clatter-test-timer)))
                  ((symbol-function 'delete-process) (lambda (&rest _))))
          (clatter--reschedule-after-resume conn)
          (should-not (clatter-connection-reconnect-enabled conn))
          (should (eq (clatter-connection-reconnect-disabled-reason conn) :user))
          (should-not scheduled))
      (clatter-test-cleanup))))

(provide 'test-handlers)

;;; test-handlers.el ends here
