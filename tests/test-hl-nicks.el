;;; test-hl-nicks.el --- Tests for nick highlighting faces -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the named-face nick coloring in clatter-hl-nicks.el.
;; These guard the rework from inline (:foreground ...) specs to real,
;; themeable faces, and ensure it does not break existing color mapping.

;;; Code:

(require 'ert)
(require 'clatter-hl-nicks)

(ert-deftest clatter-hl-urls-are-standard-buttons ()
  "URL highlighting produces a standard Emacs button."
  (let* ((text (clatter-hl-urls-in-string "see https://example.com/a"))
         (button (get-text-property (string-match "https" text) 'button text)))
    (should button)
    (should (equal (get-text-property (string-match "https" text) 'clatter-url text)
                   "https://example.com/a"))))

(ert-deftest clatter-hl-nick-index-deterministic-and-in-range ()
  "The palette index is stable and within bounds."
  (let ((n (length clatter-hl-nick-colors)))
    (should (equal (clatter-hl-nick-index "alice")
                   (clatter-hl-nick-index "alice")))
    (dolist (nick '("alice" "bob" "Carol" "knighthk" "x" ""))
      (let ((idx (clatter-hl-nick-index nick)))
        (should (integerp idx))
        (should (>= idx 0))
        (should (< idx n))))))

(ert-deftest clatter-hl-nick-index-case-insensitive ()
  "Index ignores case (matches color cache behavior)."
  (should (equal (clatter-hl-nick-index "Alice")
                 (clatter-hl-nick-index "alice"))))

(ert-deftest clatter-hl-nick-face-symbol-is-real-face ()
  "The face returned for a nick is an actually defined face."
  (clatter-hl-rebuild-nick-faces)
  (dolist (nick '("alice" "bob" "knighthk"))
    (let ((face (clatter-hl-nick-face-symbol nick)))
      (should (symbolp face))
      (should (facep face)))))

(ert-deftest clatter-hl-nick-face-symbol-stable ()
  "Same nick always maps to the same face symbol."
  (should (eq (clatter-hl-nick-face-symbol "alice")
              (clatter-hl-nick-face-symbol "alice"))))

(ert-deftest clatter-hl-nick-face-matches-palette-color ()
  "The named face foreground equals the palette color for that nick.
This is the non-breaking guarantee: colors are unchanged, only named."
  (clatter-hl-rebuild-nick-faces)
  (dolist (nick '("alice" "bob" "knighthk" "Carol"))
    (let* ((face (clatter-hl-nick-face-symbol nick))
           (expected (nth (clatter-hl-nick-index nick) clatter-hl-nick-colors)))
      (should (equal (face-attribute face :foreground nil t) expected))
      (should (equal (clatter-hl-nick-color nick) expected)))))

(ert-deftest clatter-hl-rebuild-nick-faces-covers-palette ()
  "Rebuilding defines one face per palette entry."
  (clatter-hl-rebuild-nick-faces)
  (dotimes (i (length clatter-hl-nick-colors))
    (should (facep (intern (format "clatter-nick-color-%d" i))))))

(ert-deftest clatter-hl-rebuild-nick-faces-preserves-without-force ()
  "Without FORCE, an existing customized face is not overwritten."
  (let ((face (intern "clatter-nick-color-0")))
    (clatter-hl-rebuild-nick-faces t)
    (set-face-attribute face nil :foreground "#010203")
    (clatter-hl-rebuild-nick-faces)            ; no force: must not reset
    (should (equal (face-attribute face :foreground nil t) "#010203"))
    ;; force: refreshes back to the palette color
    (clatter-hl-rebuild-nick-faces t)
    (should (equal (face-attribute face :foreground nil t)
                   (nth 0 clatter-hl-nick-colors)))))

(provide 'test-hl-nicks)

;;; test-hl-nicks.el ends here
