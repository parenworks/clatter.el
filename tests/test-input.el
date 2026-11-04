;;; test-input.el --- Tests for prompt placement and input handling -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the bottom-anchored prompt (oldest-first), the top prompt
;; (newest-first), input get/clear, and jump-to-prompt-on-type.

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'clatter-ui)
(require 'clatter-commands)
(require 'clatter-completion)
(require 'clatter-smart)

(defmacro clatter-input-test--with (order &rest body)
  "Run BODY in a fresh clatter-mode buffer with message ORDER and a prompt."
  (declare (indent 1))
  `(let ((clatter-message-order ,order))
     (with-temp-buffer
       (clatter-mode)
       (setq-local clatter--target "#test")
       (setq-local clatter--buffer-type 'channel)
       (clatter--setup-prompt (current-buffer))
       ,@body)))

(defmacro clatter-input-test--with-window (order &rest body)
  "Display a temporary Clatter buffer with message ORDER, then run BODY.
Within BODY, `buffer' and `window' name the temporary buffer and its window."
  (declare (indent 1) (debug t))
  `(let* ((clatter-message-order ,order)
          (buffer (generate-new-buffer " *clatter-input-window-test*"))
          (window (selected-window))
          (original-buffer (window-buffer window)))
     (unwind-protect
         (progn
           (set-window-buffer window buffer)
           (with-current-buffer buffer
             (clatter-mode)
             (setq-local clatter--target "#test")
             (setq-local clatter--buffer-type 'channel)
             (clatter--setup-prompt buffer)
             (clatter--refresh-input-spacers buffer)
             ,@body))
       (when (window-live-p window)
         (set-window-buffer window original-buffer))
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(defun clatter-input-test--last-line ()
  "Return the text of the buffer's last line."
  ;; The prompt is its own text field, so ignore field boundaries here to
  ;; read the whole physical line rather than just the input part of it.
  (let ((inhibit-field-text-motion t))
    (save-excursion
      (goto-char (point-max))
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))

(defun clatter-input-test--first-line ()
  "Return the text of the buffer's first line."
  (let ((inhibit-field-text-motion t))
    (save-excursion
      (goto-char (point-min))
      (buffer-substring-no-properties (line-beginning-position)
                                      (line-end-position)))))

(defun clatter-input-test--prompt ()
  "Return the current prompt without text properties."
  (buffer-substring-no-properties clatter--prompt-marker
                                  clatter--input-marker))

(defun clatter-input-test--typing-text ()
  "Return the typing separator overlay's displayed text."
  (and (overlayp clatter--typing-indicator-overlay)
       (overlay-get clatter--typing-indicator-overlay 'before-string)))

(defun clatter-input-test--spacer-lines (window)
  "Return the number of protected layout lines available to WINDOW."
  (ignore window)
  (when clatter--input-padding-end
    (count-lines (point-min) clatter--input-padding-end)))

(defun clatter-input-test--formatting-overlays ()
  "Return input-formatting overlays in the current buffer."
  (seq-filter
   (lambda (overlay) (overlay-get overlay 'clatter-input-formatting))
   (overlays-in (point-min) (point-max))))

(ert-deftest clatter-completion-nick-suffix-default ()
  "Nick completion at the start of input appends the default suffix."
  (let ((clatter-completion-nick-suffix ": "))
    (clatter-input-test--with 'oldest-first
      (setq-local clatter--nick-list (make-hash-table :test 'equal))
      (puthash "alice" '("" . "alice") clatter--nick-list)
      (goto-char clatter--input-marker)
      (insert "ali")
      (let ((capf (clatter-completion--nick-capf)))
        (should capf)
        (delete-region (nth 0 capf) (nth 1 capf))
        (insert "alice")
        (funcall (plist-get (nthcdr 3 capf) :exit-function) 'alice 'finished))
      (should (equal (clatter--get-input) "alice: ")))))

(ert-deftest clatter-completion-nick-suffix-custom ()
  "Nick completion at the start of input appends a custom suffix."
  (let ((clatter-completion-nick-suffix " / "))
    (clatter-input-test--with 'oldest-first
      (setq-local clatter--nick-list (make-hash-table :test 'equal))
      (puthash "alice" '("" . "alice") clatter--nick-list)
      (goto-char clatter--input-marker)
      (insert "ali")
      (let ((capf (clatter-completion--nick-capf)))
        (should capf)
        (delete-region (nth 0 capf) (nth 1 capf))
        (insert "alice")
        (funcall (plist-get (nthcdr 3 capf) :exit-function) 'alice 'finished))
      (should (equal (clatter--get-input) "alice / ")))))

(ert-deftest clatter-completion-nick-suffix-empty ()
  "An empty nick completion suffix inserts no extra characters."
  (let ((clatter-completion-nick-suffix ""))
    (clatter-input-test--with 'oldest-first
      (setq-local clatter--nick-list (make-hash-table :test 'equal))
      (puthash "alice" '("" . "alice") clatter--nick-list)
      (goto-char clatter--input-marker)
      (insert "ali")
      (let ((capf (clatter-completion--nick-capf)))
        (should capf)
        (delete-region (nth 0 capf) (nth 1 capf))
        (insert "alice")
        (funcall (plist-get (nthcdr 3 capf) :exit-function) 'alice 'finished))
      (should (equal (clatter--get-input) "alice")))))

(ert-deftest clatter-completion-nicks-sorted-by-activity ()
  "Most recently active nicks sort first; ties fall back alphabetical."
  (with-temp-buffer
    (setq-local clatter--nick-list (make-hash-table :test 'equal))
    (puthash "bob" '("" . "Bob") clatter--nick-list)
    (puthash "alice" '("" . "Alice") clatter--nick-list)
    (puthash "carol" '("" . "Carol") clatter--nick-list)
    ;; Bob chatted most recently, then Carol; Alice chatted most overall
    ;; but longer ago, and lurked (join only) after that.
    (clatter-smart-put (current-buffer) "alice" 'privmsg (time-subtract nil 300))
    (clatter-smart-put (current-buffer) "alice" 'privmsg (time-subtract nil 200))
    (clatter-smart-put (current-buffer) "carol" 'privmsg (time-subtract nil 60))
    (clatter-smart-put (current-buffer) "bob" 'privmsg (time-subtract nil 10))
    (should (equal (clatter-completion--nicks) '("Bob" "Carol" "Alice")))
    ;; No smart data yet: plain alphabetical.
    (setq-local clatter-smart-data nil)
    (should (equal (clatter-completion--nicks) '("Alice" "Bob" "Carol")))))

(ert-deftest clatter-prompt-format-expands-placeholders ()
  "String prompt formats expand target, nick, network, and percent."
  (let ((clatter-prompt-format "%N/%n:%t %% "))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (should (string-suffix-p "testnet/alice:#test % "
                                       (clatter-input-test--prompt))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-format-function-receives-context ()
  "Function prompt formats receive the Clatter buffer."
  (let ((clatter-prompt-format
         (lambda (buffer)
           (with-current-buffer buffer
             (let ((conn (clatter-get-connection clatter--network)))
               (format "%s@%s/%s>"
                       (clatter-connection-nick conn)
                       clatter--network
                       clatter--target))))))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (should (string-suffix-p "alice@testnet/#test>"
                                       (clatter-input-test--prompt))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-format-needs-nick-detects-unescaped-specifier ()
  "Nick prompt detection ignores literal percent escapes."
  (let ((clatter-prompt-format "%n> "))
    (should (clatter--prompt-format-needs-nick-p)))
  (let ((clatter-prompt-format "%t %%n> "))
    (should-not (clatter--prompt-format-needs-nick-p)))
  (let ((clatter-prompt-format (lambda (_buffer) "prompt> ")))
    (should (clatter--prompt-format-needs-nick-p))))

(ert-deftest clatter-prompt-nick-hides-mode-line-nick ()
  "Prompts that display the current nick do not repeat it in the mode-line."
  (let ((clatter-prompt-format "%n> "))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (should (string-suffix-p "alice> "
                                       (clatter-input-test--prompt)))
              (should-not (string-match-p "alice" (clatter--mode-line-string))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-target-keeps-mode-line-nick ()
  "Prompts without the current nick keep the nick in the mode-line."
  (let ((clatter-prompt-format "%t> "))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (should (string-suffix-p "#test> "
                                       (clatter-input-test--prompt)))
              (should (string-match-p "alice" (clatter--mode-line-string))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-refresh-preserves-pending-input ()
  "Refreshing a nick-based prompt retains typed input and input point."
  (let ((clatter-prompt-format "%n> "))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (goto-char (clatter--input-end))
              (insert "draft")
              (goto-char (+ (marker-position clatter--input-marker) 2))
              (setf (clatter-connection-nick conn) "bob")
              (clatter--refresh-prompt)
              (should (string-suffix-p "bob> "
                                       (clatter-input-test--prompt)))
              (should (equal (clatter--get-input) "draft"))
              (should (= (point) (+ (marker-position clatter--input-marker) 2))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-refresh-keeps-later-messages-above-prompt ()
  "A nick refresh must not let the next message land in the input."
  (let ((clatter-message-order 'oldest-first)
        (clatter-prompt-format "%n> ")
        (clatter-prompt-alignment 'right))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--network "testnet")
      (setq-local clatter--target "#test")
      (let ((conn (clatter-test-make-connection "testnet" "alice")))
        (unwind-protect
            (progn
              (clatter--setup-prompt (current-buffer))
              (goto-char (clatter--input-end))
              (insert "draft")
              (setf (clatter-connection-nick conn) "knighthk-irc")
              (clatter--refresh-prompt)
              (clatter--insert-message (current-buffer) "<trev> now what is HIP!" t)
              (should (string-match-p "knighthk-irc> "
                                      (clatter-input-test--prompt)))
              (should (equal (clatter--get-input) "draft"))
              (should (<= (marker-position clatter--messages-marker)
                          (marker-position clatter--prompt-marker)))
              (should (string-match-p
                       "now what is HIP!"
                       (buffer-substring-no-properties
                        (point-min) clatter--prompt-marker)))
              (should-not (string-match-p "HIP" (clatter--get-input))))
          (remhash (clatter-connection-network-id conn) clatter-connections))))))

(ert-deftest clatter-prompt-nick-hook-refreshes-nick-prompts-only ()
  "Own nick changes refresh nick prompts and preserve other prompt formats."
  (let ((conn (clatter-test-make-connection "testnet" "alice"))
        nick-buffer
        target-buffer)
    (unwind-protect
        (progn
          (setq nick-buffer (clatter-get-or-create-buffer "testnet" "#nick" 'channel))
          (with-current-buffer nick-buffer
            (setq-local clatter-prompt-format "%n> ")
            (clatter--setup-prompt nick-buffer)
            (goto-char (clatter--input-end))
            (insert "draft"))
          (setq target-buffer (clatter-get-or-create-buffer "testnet" "#target" 'channel))
          (with-current-buffer target-buffer
            (setq-local clatter-prompt-format "%t> ")
            (clatter--setup-prompt target-buffer)
            (should (string-suffix-p "#target> "
                                     (clatter-input-test--prompt))))
          (setf (clatter-connection-nick conn) "bob")
          (clatter-ui--on-nick conn (clatter-parse-prefix "alice!u@h") "bob")
          (with-current-buffer nick-buffer
            (should (string-suffix-p "bob> "
                                     (clatter-input-test--prompt)))
            (should (equal (clatter--get-input) "draft")))
          (with-current-buffer target-buffer
            (should (string-suffix-p "#target> "
                                     (clatter-input-test--prompt)))))
      (when nick-buffer
        (clatter-remove-buffer "testnet" "#nick")
        (when (buffer-live-p nick-buffer)
          (kill-buffer nick-buffer)))
      (when target-buffer
        (clatter-remove-buffer "testnet" "#target")
        (when (buffer-live-p target-buffer)
          (kill-buffer target-buffer)))
      (remhash (clatter-connection-network-id conn) clatter-connections))))

(ert-deftest clatter-prompt-default-preserves-historical-layout ()
  "The default prompt layout remains unpadded."
  (let ((clatter-message-order 'oldest-first)
        (clatter-nick-column-width 10)
        (clatter-prompt-format "%t> ")
        (clatter-prompt-alignment nil))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--target "#test")
      (clatter--setup-prompt (current-buffer))
      (should (equal (clatter-input-test--prompt) "#test> "))
      (should (= (current-column) 7)))))

(ert-deftest clatter-prompt-is-right-aligned-to-nick-column ()
  "A prompt's visible text ends at the nick column boundary."
  (let ((clatter-message-order 'oldest-first)
        (clatter-nick-column-width 10)
        (clatter-prompt-format "%t> ")
        (clatter-prompt-alignment 'right))
    (with-temp-buffer
      (clatter-mode)
      (setq-local clatter--target "#test")
      (clatter--setup-prompt (current-buffer))
      (should (equal (clatter-input-test--prompt)
                     "    #test> "))
      ;; The prompt's trailing space occupies the same separator column as a
      ;; rendered message, so typed input starts at column 11.
      (should (= (current-column) 11)))))

(ert-deftest clatter-prompt-long-text-is-preserved ()
  "Overlong nick and target prompts are neither truncated nor padded."
  (let ((clatter-message-order 'oldest-first)
        (clatter-nick-column-width 10)
        (clatter-prompt-alignment 'right)
        (conn (clatter-test-make-connection
               "testnet" "very-long-nickname")))
    (unwind-protect
        (dolist (case '(("%t> " . "#very-long-channel-name> ")
                        ("%n> " . "very-long-nickname> ")))
          (let ((clatter-prompt-format (car case)))
            (with-temp-buffer
              (clatter-mode)
              (setq-local clatter--network "testnet")
              (setq-local clatter--target "#very-long-channel-name")
              (clatter--setup-prompt (current-buffer))
              (should (equal (clatter-input-test--prompt) (cdr case)))
              (should (= (current-column) (string-width (cdr case)))))))
      (remhash (clatter-connection-network-id conn) clatter-connections))))

(ert-deftest clatter-input-oldest-first-prompt-at-bottom ()
  "Oldest-first: prompt is on the last line; messages accumulate above it."
  (clatter-input-test--with 'oldest-first
    (should (string-suffix-p "#test> " (clatter-input-test--last-line)))
    (clatter--insert-message (current-buffer) "first")
    (clatter--insert-message (current-buffer) "second")
    ;; Prompt still on the last line.
    (should (string-suffix-p "#test> " (clatter-input-test--last-line)))
    ;; Chronological order above the prompt: first, then second, then prompt.
    (should (string-match-p
             "first\nsecond\n *#test>"
             (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest clatter-input-oldest-first-pins-short-buffer-to-window-bottom ()
  "History grows upward without moving the oldest-first input row."
  (clatter-input-test--with-window 'oldest-first
    (let ((height (window-body-height window)))
      (should (> height 3))
      (should (= (clatter-input-test--spacer-lines window)
                 (1- height)))
      (should (get-text-property (point-min) 'clatter-input-padding))
      (clatter--insert-message buffer "first" t)
      (clatter--insert-message buffer "second" t)
      (should (= (clatter-input-test--spacer-lines window)
                 (1- height)))
      ;; The window start advances through protected padding as messages stack
      ;; upward, while real history begins at the padding marker.
      (let ((first-position (marker-position clatter--input-padding-end)))
        (save-excursion
          (goto-char first-position)
          (should (looking-at-p "first"))))
      (should (= (count-screen-lines (window-start window)
                                     (point-max) nil window)
                 height)))))

(ert-deftest clatter-input-newest-first-does-not-create-window-spacer ()
  "Top-prompt buffers retain their existing window layout."
  (clatter-input-test--with-window 'newest-first
    (should-not clatter--input-padding-end)
    (clatter--insert-message buffer "message" t)
    (should-not clatter--input-padding-end)))

(ert-deftest clatter-input-oldest-first-window-starts-are-independent ()
  "Split windows independently bottom-align against shared real padding."
  (clatter-input-test--with-window 'oldest-first
    (let ((other-window (split-window window nil 'below)))
      (unwind-protect
          (progn
            (set-window-buffer other-window buffer)
            (set-window-point window clatter--input-marker)
            (set-window-point other-window clatter--input-marker)
            (clatter--refresh-input-spacers buffer)
            (dolist (candidate (list window other-window))
              (should (= (count-screen-lines (window-start candidate)
                                             (point-max) nil candidate)
                         (window-body-height candidate)))))
        (when (window-live-p other-window)
          (delete-window other-window))))))

(ert-deftest clatter-input-oldest-first-short-history-stays-bottom-pinned ()
  "Moving point into a short history does not dislodge the input row."
  (clatter-input-test--with-window 'oldest-first
    (dotimes (index 4)
      (clatter--insert-message buffer (format "message-%d" index) t))
    (set-window-point window clatter--input-padding-end)
    (set-window-start window (point-min))
    (clatter--refresh-input-spacers buffer)
    (should clatter--input-padding-end)
    (clatter--insert-message buffer "incoming" t)
    (should (= (count-screen-lines (window-start window)
                                   (point-max) nil window)
               (window-body-height window)))
    (should (= (window-point window) clatter--input-padding-end))))

(ert-deftest clatter-input-oldest-first-overflowing-history-retains-viewport ()
  "A window deliberately reading overflowing history does not recenter."
  (clatter-input-test--with-window 'oldest-first
    (dotimes (index (+ (window-body-height window) 4))
      (clatter--insert-message buffer (format "message-%02d" index) t))
    (set-window-point window clatter--input-padding-end)
    (set-window-start window clatter--input-padding-end)
    (clatter--refresh-input-spacers buffer)
    (let ((start (window-start window)))
      (clatter--insert-message buffer "incoming" t)
      (should (= (window-start window) start))
      (should (= (window-point window) clatter--input-padding-end)))))

(ert-deftest clatter-input-oldest-first-overflow-check-is-bounded ()
  "Refreshing a long history does not scan it from beginning to end."
  (clatter-input-test--with-window 'oldest-first
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (save-excursion
        (goto-char clatter--messages-marker)
        (dotimes (index 2000)
          (insert (format "message-%04d\n" index)))))
    (set-window-point window clatter--input-padding-end)
    (set-window-start window clatter--input-padding-end)
    (let ((history-start (marker-position clatter--input-padding-end))
          (history-end (point-max))
          (original-count-screen-lines (symbol-function 'count-screen-lines)))
      (cl-letf (((symbol-function 'count-screen-lines)
                 (lambda (&optional beg end count-final-newline candidate-window)
                   (when (and beg end
                              (= beg history-start)
                              (= end history-end))
                     (ert-fail "Refreshed by scanning the complete history"))
                   (funcall original-count-screen-lines
                            beg end count-final-newline candidate-window))))
        (clatter--refresh-input-spacers buffer)))))

(ert-deftest clatter-input-oldest-first-overflow-keeps-input-point-at-bottom ()
  "A full following window scrolls minimally without disturbing draft input."
  (clatter-input-test--with-window 'oldest-first
    (goto-char (clatter--input-end))
    (insert "draft")
    (clatter--refresh-input-spacers buffer)
    (let ((height (window-body-height window)))
      (dotimes (index (+ height 3))
        (clatter--insert-message buffer (format "message-%02d" index) t))
      (should (equal (clatter--get-input) "draft"))
      (should (= (- (window-point window)
                    (marker-position clatter--input-marker))
                 (length "draft")))
      (should (= (count-screen-lines (window-start window)
                                     (point-max) nil window)
                 height)))))

(ert-deftest clatter-input-newest-first-prompt-at-top ()
  "Newest-first: prompt is on the first line; newest message sits just below."
  (clatter-input-test--with 'newest-first
    (should (string-suffix-p "#test> " (clatter-input-test--first-line)))
    (clatter--insert-message (current-buffer) "first")
    (clatter--insert-message (current-buffer) "second")
    (should (string-suffix-p "#test> " (clatter-input-test--first-line)))
    ;; Newest (second) is directly below the prompt, older (first) below it.
    (should (string-match-p
             "#test>.*\nsecond\nfirst\n"
             (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest clatter-input-typing-separator-reserves-row-in-both-orders ()
  "The input-separator placement keeps one blank row beside the prompt."
  (let ((clatter-typing-indicator-location 'input-separator))
    (dolist (order '(oldest-first newest-first))
      (clatter-input-test--with order
        (should (overlayp clatter--typing-indicator-overlay))
        (should (get-text-property (overlay-start clatter--typing-indicator-overlay)
                                   'clatter-typing-indicator-line))
        (should-not (clatter-input-test--typing-text))
        (clatter--insert-message (current-buffer) "first" t)
        (clatter--insert-message (current-buffer) "second" t)
        (let ((rendered (buffer-substring-no-properties (point-min) (point-max))))
          (should
           (string-match-p
            (if (eq order 'oldest-first)
                "first\nsecond\n\n *#test>"
              "#test>.*\n\nsecond\nfirst\n")
            rendered)))))))

(ert-deftest clatter-input-typing-refresh-preserves-draft-and-undo ()
  "Typing display changes never alter draft text, prompt, or undo positions."
  (let ((clatter-typing-indicator-location 'input-separator))
    (dolist (order '(oldest-first newest-first))
      (clatter-input-test--with order
        (buffer-enable-undo)
        (goto-char (clatter--input-end))
        (setq buffer-undo-list nil)
        (insert "draft")
        (let ((prompt (clatter-input-test--prompt))
              (text (buffer-string)))
          (setq-local clatter--typing-nicks (make-hash-table :test 'equal))
          (puthash "alice" t clatter--typing-nicks)
          (clatter--refresh-typing-indicator)
          (should (equal (buffer-string) text))
          (should (equal (clatter--get-input) "draft"))
          (should (equal (clatter-input-test--prompt) prompt))
          (should (= (field-end clatter--input-marker) (clatter--input-end)))
          (primitive-undo 1 buffer-undo-list)
          (should (equal (clatter--get-input) ""))
          (clatter--set-input "draft")
          (clatter--refresh-prompt)
          (should (equal (clatter--get-input) "draft"))
          (should (overlayp clatter--typing-indicator-overlay))
          (clatter--insert-message (current-buffer) "after-refresh" t)
          (should (equal (clatter--get-input) "draft"))
          (should-not (string-match-p "after-refresh" (clatter--get-input))))))))

(ert-deftest clatter-input-typing-separator-bottom-pin-reserves-one-row ()
  "Oldest-first bottom pin accounts for the fixed typing row."
  (let ((clatter-typing-indicator-location 'input-separator))
    (clatter-input-test--with-window 'oldest-first
      (let ((height (window-body-height window)))
        (should (> height 3))
        (should (= (clatter-input-test--spacer-lines window)
                   (- height 2)))
        (clatter--insert-message buffer "first" t)
        (should (= (count-screen-lines (window-start window)
                                       (point-max) nil window)
                   height))))))

(ert-deftest clatter-input-clear-preserves-typing-separator ()
  "/clear removes history but retains the typing row and prompt."
  (let ((clatter-typing-indicator-location 'input-separator))
    (clatter-input-test--with 'oldest-first
      (clatter--insert-message (current-buffer) "message" t)
      (let ((overlay clatter--typing-indicator-overlay))
        (clatter-cmd-clear nil)
        (should (eq overlay clatter--typing-indicator-overlay))
        (should (overlay-buffer overlay))
        (should (equal (buffer-string) "\n#test> "))))))

(ert-deftest clatter-input-clear-discards-history-preserves-draft-undo ()
  "/clear disposes timestamps and history without losing draft undo or padding."
  (let ((clatter-typing-indicator-location 'input-separator)
        (clatter-timestamp-side 'left)
        (clatter-timestamp-only-if-changed nil)
        (clatter-buffer-max-lines nil))
    (dolist (order '(oldest-first newest-first))
      (clatter-input-test--with order
        (when (eq order 'oldest-first)
          (clatter--ensure-input-padding 3))
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (goto-char (clatter--input-end))
        (insert "draft")
        (undo-boundary)
        (delete-region (1- (point)) (point))
        (clatter--insert-message (current-buffer) "discarded-history-one")
        (clatter--insert-message (current-buffer) "discarded-history-two")
        (let* ((prompt (clatter-input-test--prompt))
               (padding (and clatter--input-padding-end
                             (buffer-substring (point-min)
                                               clatter--input-padding-end)))
               (typing-overlay clatter--typing-indicator-overlay)
               (timestamps (seq-filter
                            (lambda (overlay)
                              (overlay-get overlay 'clatter-timestamp))
                            (overlays-in (point-min) (point-max))))
               (draft-overlay (make-overlay clatter--input-marker
                                            (1+ clatter--input-marker)))
               (history-start (if (eq order 'oldest-first)
                                  clatter--input-padding-end
                                clatter--messages-marker))
               (unrelated-overlay (make-overlay history-start
                                                (1+ history-start))))
          (should (= (length timestamps) 2))
          (clatter-cmd-clear nil)
          (should (equal (clatter--get-input) "draf"))
          (should (equal (clatter-input-test--prompt) prompt))
          (when padding
            (should (equal (buffer-substring (point-min)
                                            clatter--input-padding-end)
                           padding)))
          (should (= (if (eq order 'oldest-first)
                         clatter--input-padding-end
                       (point-max))
                     clatter--messages-marker))
          (should-not (string-match-p "discarded-history" (buffer-string)))
          (dolist (overlay timestamps)
            (should-not (overlay-buffer overlay)))
          (dolist (overlay (list typing-overlay draft-overlay unrelated-overlay))
            (should (eq (overlay-buffer overlay) (current-buffer))))
          ;; Only the deleted draft character, never history, is retained.
          (should (equal (cl-loop for entry in buffer-undo-list
                                  when (and (consp entry) (stringp (car entry)))
                                  collect (car entry))
                         '("t")))
          (let ((pending (primitive-undo 1 buffer-undo-list)))
            (should (equal (clatter--get-input) "draft"))
            (primitive-undo 1 pending)
            (should (equal (clatter--get-input) "")))
          (should (equal (clatter-input-test--prompt) prompt)))))))

(ert-deftest clatter-input-truncation-preserves-typing-separator ()
  "History limits count messages only and never delete the typing row."
  (let ((clatter-typing-indicator-location 'input-separator)
        (clatter-buffer-max-lines 2))
    (dolist (order '(oldest-first newest-first))
      (clatter-input-test--with order
        (dolist (text '("message-1" "message-2" "message-3" "message-4"))
          (clatter--insert-message (current-buffer) text t))
        (let ((rendered (buffer-string)))
          (should-not (string-match-p "message-[12]" rendered))
          (should (string-match-p "message-3" rendered))
          (should (string-match-p "message-4" rendered))
          (should (overlay-buffer clatter--typing-indicator-overlay))
          (should (get-text-property
                   (overlay-start clatter--typing-indicator-overlay)
                   'clatter-typing-indicator-line)))))))

(ert-deftest clatter-input-get-and-clear-oldest ()
  "Input get/clear work with a bottom prompt, even after messages arrive."
  (clatter-input-test--with 'oldest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (point-max))
    (insert "hello world")
    (should (equal (clatter--get-input) "hello world"))
    (should (= (clatter--input-end) (point-max)))
    (clatter--clear-input)
    (should (equal (clatter--get-input) ""))
    ;; The message above the prompt is untouched.
    (should (string-match-p "noise" (buffer-string)))))

(ert-deftest clatter-input-get-and-clear-newest ()
  "Input get/clear work with a top prompt."
  (clatter-input-test--with 'newest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (marker-position clatter--input-marker))
    (insert "hello")
    (should (equal (clatter--get-input) "hello"))
    (clatter--clear-input)
    (should (equal (clatter--get-input) ""))))

(ert-deftest clatter-input-move-to-prompt-oldest ()
  "Self-inserting from the message area jumps to the input."
  (clatter-input-test--with 'oldest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (point-min))                 ; up in the messages
    (let ((this-command 'self-insert-command)
          (clatter-move-to-prompt t))
      (clatter--move-to-prompt)
      (should (= (point) (clatter--input-end))))))

(ert-deftest clatter-input-move-to-prompt-disabled ()
  "With `clatter-move-to-prompt' nil, point is left alone."
  (clatter-input-test--with 'oldest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (point-min))
    (let ((this-command 'self-insert-command)
          (clatter-move-to-prompt nil))
      (clatter--move-to-prompt)
      (should (= (point) (point-min))))))

(ert-deftest clatter-input-move-to-prompt-not-self-insert ()
  "Non-self-insert commands never move point."
  (clatter-input-test--with 'oldest-first
    (clatter--insert-message (current-buffer) "noise")
    (goto-char (point-min))
    (let ((this-command 'next-line)
          (clatter-move-to-prompt t))
      (clatter--move-to-prompt)
      (should (= (point) (point-min))))))

(ert-deftest clatter-input-undo-survives-incoming-message ()
  "Undo of typed input is not corrupted by a message inserted above it.
Regression for the rcirc-style bug: a bottom-anchored prompt shifts the
input down when messages arrive, so undo must shift its recorded
positions or it deletes the wrong (message) text."
  (clatter-input-test--with 'oldest-first
    (buffer-enable-undo)
    (goto-char (clatter--input-end))
    (setq buffer-undo-list nil)
    (insert "hello world")
    (should (equal (clatter--get-input) "hello world"))
    ;; A message arrives and pushes the input down.
    (clatter--insert-message (current-buffer) "<bob> incoming line here")
    (should (equal (clatter--get-input) "hello world"))
    ;; Undo the typing: it must remove the input and leave the message.
    (primitive-undo 1 buffer-undo-list)
    (should (equal (clatter--get-input) ""))
    (should (string-match-p "incoming line here" (buffer-string)))))

(ert-deftest clatter-update-undo-list-shifts-positions ()
  "`clatter--update-undo-list' shifts integer positions and (BEG . END)."
  (with-temp-buffer
    (let ((buffer-undo-list (list 10 (cons 5 8) (cons "txt" 12) nil)))
      (clatter--update-undo-list 3)
      (should (equal (nth 0 buffer-undo-list) 13))      ; POSITION
      (should (equal (nth 1 buffer-undo-list) (cons 8 11)))  ; (BEG . END)
      (should (equal (nth 2 buffer-undo-list) (cons "txt" 15))) ; (TEXT . POS)
      (should (null (nth 3 buffer-undo-list))))))       ; boundary untouched

(ert-deftest clatter-update-undo-list-noop-on-zero ()
  "A zero shift leaves the undo list untouched."
  (let ((buffer-undo-list (list 10 (cons 5 8))))
    (clatter--update-undo-list 0)
    (should (equal buffer-undo-list (list 10 (cons 5 8))))))

(ert-deftest clatter-input-formatting-styles ()
  "Style commands wrap selected input and render in both prompt orders."
  (dolist (order '(oldest-first newest-first))
    (dolist (spec
             `(("b" clatter-input-bold ,clatter-format--bold (:weight bold))
               ("i" clatter-input-italic ,clatter-format--italic (:slant italic))
               ("u" clatter-input-underline ,clatter-format--underline
                (:underline t))
               ("s" clatter-input-strikethrough
                ,clatter-format--strikethrough (:strike-through t))
               ("m" clatter-input-monospace ,clatter-format--monospace
                (:family "Monospace"))
               ("v" clatter-input-reverse ,clatter-format--reverse
                (:inverse-video t))))
      (pcase-let ((`(,key ,command ,control ,face) spec))
        (clatter-input-test--with order
          (buffer-enable-undo)
          (should clatter-input-formatting-mode)
          (clatter--set-input "word")
          (setq buffer-undo-list nil
                transient-mark-mode t)
          (goto-char clatter--input-marker)
          (push-mark (clatter--input-end) t t)
          (call-interactively command)
          (let ((raw (concat (string control) "word" (string control)))
                (overlays (clatter-input-test--formatting-overlays)))
            (should (eq (lookup-key clatter-input-formatting-mode-map
                                    (kbd (concat "C-c C-f " key)))
                        command))
            (should (equal (clatter--get-input) raw))
            (should (= (length overlays) 3))
            (should (= (seq-count
                        (lambda (overlay)
                          (equal (overlay-get overlay 'display) ""))
                        overlays)
                       2))
            (should (equal
                     (get-char-property
                      (1+ (marker-position clatter--input-marker)) 'face)
                     face))
            (clatter-input-formatting-mode -1)
            (should-not (clatter-input-test--formatting-overlays))
            (should (equal (clatter--get-input) raw))
            (clatter-input-formatting-mode 1)
            (primitive-undo 1 buffer-undo-list)
            (should (equal (clatter--get-input) "word"))))))))

(ert-deftest clatter-input-formatting-color ()
  "Indexed colors wrap input and update their visible extent after edits."
  (should (equal (clatter-format--color-name-for-index 0) "white"))
  (should (equal (clatter-format--color-name-for-index 16) "dark red"))
  (should (equal (clatter-format--color-name-for-index 87) "pale pink"))
  (should (equal (clatter-format--color-name-for-index 88) "black"))
  (should (equal (clatter-format--color-name-for-index 98) "white"))
  (clatter-input-test--with 'oldest-first
    (should clatter-input-formatting-mode)
    (clatter--set-input "word tail")
    (setq transient-mark-mode t)
    (goto-char clatter--input-marker)
    (push-mark (+ (marker-position clatter--input-marker) 4) t t)
    (let ((answers '("04 light red #ff0000" "02 blue #00007f")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (let* ((answer (pop answers))
                          (annotation
                           (plist-get completion-extra-properties
                                      :annotation-function))
                          (swatch (funcall annotation answer)))
                     (should (member answer collection))
                     (should
                      (equal (plist-get (get-text-property 1 'face swatch)
                                        :background)
                             (clatter-format--color-for-index
                              (string-to-number answer))))
                     answer))))
        (clatter-input-color)))
    (let* ((input-start (marker-position clatter--input-marker))
           (face '(:foreground "#ff0000" :background "#00007f"))
           (raw (concat (string clatter-format--color)
                        "04,02word" (string clatter-format--color) " tail")))
      (should (equal (clatter--get-input) raw))
      (should (equal (get-char-property (+ input-start 6) 'face) face))
      (should-not (get-char-property (+ input-start 12) 'face))
      (goto-char (+ input-start 10))
      (delete-char 1)
      (should (equal (get-char-property (+ input-start 11) 'face) face))
      (insert (string clatter-format--color))
      (should-not (get-char-property (+ input-start 12) 'face)))
    (clatter--set-input "word")
    (goto-char clatter--input-marker)
    (push-mark (clatter--input-end) t t)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (member "default" collection))
                 "default")))
      (clatter-input-color))
    (should
     (equal (clatter--get-input)
            (concat (string clatter-format--color)
                    "word" (string clatter-format--color))))))

(ert-deftest clatter-input-formatting-wire-preserves-controls ()
  "Input overlays leave history and the sent IRC payload unchanged."
  (let* ((clatter-send-typing nil)
         (conn (clatter-test-make-connection "testnet" "me"))
         (raw (concat (string clatter-format--bold)
                      "bold"
                      (string clatter-format--bold)
                      " "
                      (string clatter-format--color)
                      "04,02color"
                      (string clatter-format--color))))
    (unwind-protect
        (with-temp-buffer
          (clatter-mode)
          (setq-local clatter--network "testnet"
                      clatter--target "#test"
                      clatter--buffer-type 'channel)
          (clatter--setup-prompt (current-buffer))
          (should clatter-input-formatting-mode)
          (goto-char clatter--input-marker)
          (insert raw)
          (should (equal (clatter--get-input) raw))
          (clatter-test-with-mock-send
            (clatter-send-input)
            (should
             (equal (clatter-test-last-sent)
                    (concat "PRIVMSG #test :" raw))))
          (should (equal (clatter-input-ring-nth 0) raw)))
      (remhash (clatter-connection-network-id conn) clatter-connections))))

;; --- Slash command dispatch ---

(defmacro clatter-cmd-test--with-channel (target &rest body)
  "Run BODY in a temp clatter buffer on network \"testnet\" with TARGET.
A mock connection is registered; `clatter-send' is mocked to capture lines."
  (declare (indent 1))
  `(let ((conn (clatter-test-make-connection "testnet" "alice")))
     (unwind-protect
         (with-temp-buffer
           (clatter-mode)
           (setq-local clatter--network "testnet")
           (setq-local clatter--target ,target)
           (clatter-test-with-mock-send
             ,@body))
       (remhash (clatter-connection-network-id conn) clatter-connections))))

(ert-deftest clatter-cmd-raw-echoes-sent-line ()
  "/raw echoes the sent line into the originating buffer."
  (let ((conn (clatter-test-make-connection "testnet" "alice")))
    (unwind-protect
        (with-temp-buffer
          (clatter-mode)
          (setq-local clatter--network "testnet")
          (setq-local clatter--target "#test")
          (clatter--setup-prompt (current-buffer))
          (let (sys)
            (cl-letf (((symbol-function 'clatter-send)
                       (lambda (&rest _) t))
                      ((symbol-function 'clatter-insert-system)
                       (lambda (_buffer text &optional _invisible)
                         (push text sys))))
              (clatter-cmd-raw "PING irc.libera.chat"))
            (should (equal (car sys) ">> PING irc.libera.chat"))))
      (remhash (clatter-connection-network-id conn) clatter-connections))))

(ert-deftest clatter-cmd-raw-no-args-errors ()
  "/raw with no args shows usage and sends nothing."
  (clatter-cmd-test--with-channel "#test"
    (let (err)
      (cl-letf (((symbol-function 'clatter-insert-error)
                 (lambda (_buffer text) (push text err))))
        (clatter-cmd-raw ""))
      (should (null clatter-test--sent-lines))
      (should (string-match-p "Usage" (car err))))))

(ert-deftest clatter-cmd-mode-explicit-channel-not-duplicated ()
  "/mode #chan +b in a channel buffer must not send the channel twice.
Sending \"MODE #chan #chan +b\" makes the server read the second channel
as the mode string and reply 472 (unknown mode char)."
  (clatter-cmd-test--with-channel "#chan"
    (clatter-cmd-mode "#chan +b")
    (should (equal (clatter-test-last-sent) "MODE #chan +b"))))

(ert-deftest clatter-cmd-mode-implicit-channel-uses-buffer-target ()
  "/mode +b in a channel buffer targets that channel.
A leading + is also a valid channel prefix, so the mode string must not
be mistaken for an explicit channel argument."
  (clatter-cmd-test--with-channel "#chan"
    (clatter-cmd-mode "+b")
    (should (equal (clatter-test-last-sent) "MODE #chan +b"))))

(ert-deftest clatter-cmd-mode-explicit-other-channel ()
  "/mode #other +b mask targets the named channel, not the buffer's."
  (clatter-cmd-test--with-channel "#chan"
    (clatter-cmd-mode "#other +b mask!*@*")
    (should (equal (clatter-test-last-sent) "MODE #other +b mask!*@*"))))

(ert-deftest clatter-cmd-mode-bare-in-channel-queries-channel ()
  "/mode with no args in a channel buffer queries that channel's modes."
  (clatter-cmd-test--with-channel "#chan"
    (clatter-cmd-mode "")
    (should (equal (clatter-test-last-sent) "MODE #chan"))))

(ert-deftest clatter-cmd-op-groups-modes ()
  "/op with several nicks sends a grouped MODE line."
  (clatter-cmd-test--with-channel "#chan"
    (clatter-cmd-op "alice bob carol")
    (should (equal (clatter-test-last-sent) "MODE #chan +ooo alice bob carol"))))

(ert-deftest clatter-cmd-op-single ()
  "/op with one nick sends a single +o."
  (clatter-cmd-test--with-channel "#chan"
    (clatter-cmd-op "alice")
    (should (equal (clatter-test-last-sent) "MODE #chan +o alice"))))

(ert-deftest clatter-cmd-op-no-nicks-errors ()
  "/op with no nicks shows usage and sends nothing."
  (clatter-cmd-test--with-channel "#chan"
    (let (err)
      (cl-letf (((symbol-function 'clatter-insert-error)
                 (lambda (_buffer text) (push text err))))
        (clatter-cmd-op ""))
      (should (null clatter-test--sent-lines))
      (should (string-match-p "Usage" (car err))))))

(ert-deftest clatter-cmd-op-in-server-buffer-errors ()
  "/op outside a channel buffer shows usage and sends nothing."
  (clatter-cmd-test--with-channel "*server*"
    (let (err)
      (cl-letf (((symbol-function 'clatter-insert-error)
                 (lambda (_buffer text) (push text err))))
        (clatter-cmd-op "alice"))
      (should (null clatter-test--sent-lines))
      (should (string-match-p "channel buffer" (car err))))))

(ert-deftest clatter-cmd-ban-wildcards-bare-nick ()
  "/ban wildcards a bare nick to nick!*@*."
  (clatter-cmd-test--with-channel "#chan"
    (clatter-cmd-ban "alice")
    (should (equal (clatter-test-last-sent) "MODE #chan +b alice!*@*"))))

(ert-deftest clatter-cmd-ban-keeps-mask ()
  "/ban leaves an explicit mask unchanged."
  (clatter-cmd-test--with-channel "#chan"
    (clatter-cmd-ban "*!*@evil.host")
    (should (equal (clatter-test-last-sent) "MODE #chan +b *!*@evil.host"))))

(ert-deftest clatter-cmd-server-queries-send-expected-lines ()
  "Server-query commands send the right protocol lines."
  (clatter-cmd-test--with-channel "#chan"
    (clatter-cmd-who "")
    (should (equal (clatter-test-last-sent) "WHO"))
    (clatter-cmd-who "#chan")
    (should (equal (clatter-test-last-sent) "WHO #chan"))
    (clatter-cmd-motd "")
    (should (equal (clatter-test-last-sent) "MOTD"))
    (clatter-cmd-stats "u")
    (should (equal (clatter-test-last-sent) "STATS u"))
    (clatter-cmd-stats "u irc.net")
    (should (equal (clatter-test-last-sent) "STATS u irc.net"))
    (clatter-cmd-ison "a b")
    (should (equal (clatter-test-last-sent) "ISON a b"))))

(ert-deftest clatter-backward-kill-word-stops-at-input-origin ()
  "\\[clatter-backward-kill-word] never kills or moves past the prompt."
  (dolist (order '(oldest-first newest-first))
    (clatter-input-test--with order
      (goto-char clatter--input-marker)
      (insert "hello world")
      (clatter-backward-kill-word 1)
      (should (equal (clatter--get-input) "hello "))
      (clatter-backward-kill-word 1)
      (should (equal (clatter--get-input) ""))
      (should (= (point) (marker-position clatter--input-marker)))
      ;; At the origin there is nothing left to kill: point must stay put.
      (clatter-backward-kill-word 1)
      (should (= (point) (marker-position clatter--input-marker)))
      (should (equal (clatter--get-input) "")))))

(ert-deftest clatter-kill-word-stops-at-input-end ()
  "\\[clatter-kill-word] never kills or moves past the end of the input."
  (dolist (order '(oldest-first newest-first))
    (clatter-input-test--with order
      (goto-char clatter--input-marker)
      (insert "foo bar")
      (goto-char (clatter--input-end))
      (clatter-kill-word 1)
      (should (equal (clatter--get-input) "foo bar"))
      (should (= (point) (clatter--input-end)))
      (goto-char clatter--input-marker)
      (clatter-kill-word 1)
      (should (equal (clatter--get-input) " bar")))))

(ert-deftest clatter-input-is-its-own-text-field ()
  "The prompt is a separate field, so line motion stops at the input."
  (dolist (order '(oldest-first newest-first))
    (clatter-input-test--with order
      (goto-char clatter--input-marker)
      (insert "hello world")
      (goto-char (- (point) 3))
      (should (= (field-beginning) (marker-position clatter--input-marker)))
      (should (= (field-end) (clatter--input-end)))
      (should (= (line-beginning-position)
                 (marker-position clatter--input-marker)))
      (should (= (line-end-position) (clatter--input-end)))
      (should (= (save-excursion (beginning-of-line) (point))
                 (marker-position clatter--input-marker)))
      (should (= (save-excursion (move-beginning-of-line nil) (point))
                 (marker-position clatter--input-marker))))))

(ert-deftest clatter-input-line-kill-never-touches-the-prompt ()
  "Line-oriented kills bounded by the field leave the buffer structure intact.
This is the shape third-party commands such as evil's `dd' use; before the
input became its own field they expanded over the read-only prompt and
signalled `text-read-only'."
  (dolist (order '(oldest-first newest-first))
    (clatter-input-test--with order
      (let ((prompt (clatter-input-test--prompt))
            (messages (marker-position clatter--messages-marker)))
        (goto-char clatter--input-marker)
        (insert "hello world")
        (goto-char (- (point) 3))
        ;; `evil-define-motion' narrows to the field before expanding a
        ;; linewise range, so the range can never leave the input area.
        (save-restriction
          (narrow-to-region (field-beginning) (field-end))
          (delete-region (line-beginning-position) (point-max)))
        (should (equal (clatter--get-input) ""))
        (should (equal (clatter-input-test--prompt) prompt))
        (should (= (marker-position clatter--messages-marker) messages))
        (should (= (point) (marker-position clatter--input-marker)))))))

(ert-deftest clatter-input-kill-line-stops-at-input-end ()
  "\\[kill-line] from the input origin clears the input and nothing else."
  (dolist (order '(oldest-first newest-first))
    (clatter-input-test--with order
      (let ((messages (marker-position clatter--messages-marker)))
        (goto-char clatter--input-marker)
        (insert "hello world")
        (goto-char clatter--input-marker)
        (kill-line)
        (should (equal (clatter--get-input) ""))
        (should (= (marker-position clatter--messages-marker) messages))))))

(ert-deftest clatter-input-flyspell-changes-stay-bounded ()
  "Message inserts prune flyspell's per-change records in background buffers.
Flyspell records a cons for every buffer modification, and only a user
command in the buffer drains them; without pruning, hours of process-filter
inserts leak hundreds of MB."
  (require 'flyspell)
  (dolist (order '(oldest-first newest-first))
    (clatter-input-test--with order
      (setq-local flyspell-mode t) ; state only; never spawns a speller here
      (setq-local flyspell-changes nil)
      ;; Programmatic inserts leave no records behind.
      (dotimes (i 50)
        ;; Simulate the after-change record a message insert would produce.
        (let ((m (marker-position clatter--messages-marker)))
          (push (cons m (1+ m)) flyspell-changes))
        (clatter--insert-message (current-buffer) (format "message-%d" i) t))
      (should (null flyspell-changes))
      ;; A record covering the current input area survives a prune.
      (goto-char clatter--input-marker)
      (insert "typo")
      (let ((input-record (cons (marker-position clatter--input-marker)
                                (clatter--input-end))))
        (setq flyspell-changes (list (cons 1 2) input-record))
        (clatter--flyspell-prune-changes)
        (should (equal flyspell-changes (list input-record)))))))

;; --- Disconnected buffers ---

(ert-deftest clatter-send-while-disconnected-warns-and-drops ()
  "Sending a plain message in a disconnected buffer inserts a warning
and sends nothing to the server."
  (with-temp-buffer
    (clatter-mode)
    (setq-local clatter--network "testnet")
    (setq-local clatter--target "#test")
    (let ((conn (clatter-test-make-connection "testnet" "alice")))
      (unwind-protect
          (progn
            (clatter--setup-prompt (current-buffer))
            (setf (clatter-connection-state conn) :disconnected)
            (goto-char clatter--input-marker)
            (insert "are you there?")
            (let (errors)
              (cl-letf (((symbol-function 'clatter-insert-error)
                         (lambda (_buf text) (push text errors))))
                (clatter-test-with-mock-send
                 (clatter-send-input)
                 (should (null clatter-test--sent-lines))))
              (should (string-match-p "Not connected to testnet"
                                      (car errors)))))
        (remhash (clatter-connection-network-id conn)
                 clatter-connections)))))

(ert-deftest clatter-require-conn-rejects-disconnected ()
  "`clatter--require-conn' refuses a down session, naming the state."
  (with-temp-buffer
    (clatter-mode)
    (setq-local clatter--network "testnet")
    (let ((conn (clatter-test-make-connection "testnet" "alice")))
      (unwind-protect
          (progn
            (setf (clatter-connection-state conn) :disconnected)
            (let (errors)
              (cl-letf (((symbol-function 'clatter-insert-error)
                         (lambda (_buf text) (push text errors))))
                (should-not (clatter--require-conn))
                (should (equal (car errors)
                               "Not connected (disconnected)"))))
            (setf (clatter-connection-state conn) :connecting)
            (let (errors)
              (cl-letf (((symbol-function 'clatter-insert-error)
                         (lambda (_buf text) (push text errors))))
                (should-not (clatter--require-conn))
                (should (equal (car errors)
                               "Not connected (connecting)")))))
        (remhash (clatter-connection-network-id conn)
                 clatter-connections)))))

(provide 'test-input)

;;; test-input.el ends here
