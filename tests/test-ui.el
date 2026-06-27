;;; test-ui.el --- Tests for clatter-ui.el -*- lexical-binding: t; -*-

;;; Code:

(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-nicklist)

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

;; --- Typing mode-line ---

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

;; --- Nicklist hooks registered ---

(ert-deftest clatter-test-nicklist-hooks-registered ()
  "Nicklist auto-refresh hooks are registered."
  (should (memq #'clatter-nicklist--on-join (default-value 'clatter-join-hook)))
  (should (memq #'clatter-nicklist--on-part (default-value 'clatter-part-hook)))
  (should (memq #'clatter-nicklist--on-quit (default-value 'clatter-quit-hook)))
  (should (memq #'clatter-nicklist--on-nick (default-value 'clatter-nick-hook)))
  (should (memq #'clatter-nicklist--on-names (default-value 'clatter-names-hook))))

(provide 'test-ui)

;;; test-ui.el ends here
