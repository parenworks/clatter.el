;;; clatter-hl-nicks.el --- Nick highlighting in message text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Enhanced nick colorization for clatter.el.
;; Highlights nicknames wherever they appear in message text,
;; using stable hash-based colors consistent across sessions.
;; Inspired by erc-hl-nicks but built-in and IRCv3-aware.

;;; Code:

(require 'button)

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-model)
(require 'clatter-format)
(require 'clatter-pals)

;; --- Configuration ---

(defcustom clatter-hl-nicks-enabled t
  "Enable in-text nick highlighting."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-hl-nicks-minimum-length 3
  "Minimum nick length to highlight in message text.
Short nicks produce too many false positives."
  :type 'integer
  :group 'clatter)

(defcustom clatter-hl-nicks-skip-nicks nil
  "List of nicks to never highlight in message text."
  :type '(repeat string)
  :group 'clatter)

(defcustom clatter-hl-nicks-skip-faces '(clatter-timestamp clatter-system clatter-notice)
  "Faces that should not receive nick highlighting."
  :type '(repeat symbol)
  :group 'clatter)

(defcustom clatter-hl-nicks-alias-alist nil
  "Alist mapping nick aliases to canonical nicks for color consistency.
Example: ((\"beach_\" . \"beach\") (\"jackdaniel_\" . \"jackdaniel\"))
Both the alias and canonical nick will get the same color."
  :type '(alist :key-type string :value-type string)
  :group 'clatter)

;; --- Keyword Highlighting ---

(defcustom clatter-hl-keywords nil
  "List of keywords to highlight in message text.
Each entry is a string.  Matching is case-insensitive."
  :type '(repeat string)
  :group 'clatter)

(defface clatter-hl-keyword
  '((t :background "#4a3000" :foreground "#ffcb6b" :weight bold))
  "Face for highlighted keywords in messages."
  :group 'clatter)

(defun clatter-hl-keywords-in-string (text)
  "Highlight keywords from `clatter-hl-keywords' in TEXT."
  (if (or (null clatter-hl-keywords) (string-empty-p text))
      text
    (let ((result text))
      (dolist (keyword clatter-hl-keywords)
        (let ((case-fold-search t)
              (pattern (regexp-quote keyword))
              (pos 0)
              (new-result ""))
          (while (string-match (concat "\\b" pattern "\\b") result pos)
            (let ((start (match-beginning 0))
                  (end (match-end 0)))
              (setq new-result (concat new-result
                                       (substring result pos start)
                                       (propertize (substring result start end)
                                                   'face 'clatter-hl-keyword)))
              (setq pos end)))
          (setq new-result (concat new-result (substring result pos)))
          (setq result new-result)))
      result)))

;; --- Expanded color palette ---

(defcustom clatter-hl-nick-colors
  '("#f78c6c" "#c3e88d" "#89ddff" "#c792ea" "#ffcb6b"
    "#ff5370" "#82aaff" "#f07178" "#babed8" "#a6accd"
    "#e2b93d" "#addb67" "#7fdbca" "#ef5350" "#80cbc4"
    "#b2ccd6" "#eeffff" "#d4bfff" "#ffd580" "#bae67e"
    "#5ccfe6" "#f29e74" "#d9f5dd" "#ffa7c4" "#c4e88e"
    "#73d0ff" "#ff6e6e" "#ffe66d" "#a9dc76" "#78dce8"
    "#ab9df2" "#fc9867" "#b8e986" "#ffd866" "#ff6188"
    "#a9dc76" "#78dce8" "#ab9df2" "#e5c07b" "#56b6c2")
  "Extended color palette for nick highlighting.
40 colors chosen for good contrast on dark backgrounds."
  :type '(repeat color)
  :group 'clatter)

;; --- Color cache ---

(defvar clatter--nick-color-cache (make-hash-table :test 'equal)
  "Cache of nick -> color mappings for fast lookup.")

(defun clatter-hl-nick-index (nick)
  "Return the palette index for NICK (deterministic, hash-based).
Uses the canonical nick from `clatter-hl-nicks-alias-alist' if present."
  (let* ((canonical (or (cdr (assoc nick clatter-hl-nicks-alias-alist))
                        nick))
         (hash (cl-reduce #'+ (mapcar #'identity (downcase canonical)))))
    (mod hash (length clatter-hl-nick-colors))))

(defun clatter-hl-nick-color (nick)
  "Return a stable color for NICK.
Uses canonical nick from `clatter-hl-nicks-alias-alist' if present.
Colors are deterministic based on a hash of the nick string."
  (let* ((canonical (or (cdr (assoc nick clatter-hl-nicks-alias-alist))
                        nick))
         (cached (gethash canonical clatter--nick-color-cache)))
    (or cached
        (let ((color (nth (clatter-hl-nick-index nick) clatter-hl-nick-colors)))
          (puthash canonical color clatter--nick-color-cache)
          color))))

;; --- Named nick faces ---
;;
;; Each palette color is exposed as a real, named face
;; (`clatter-nick-color-0' ... `clatter-nick-color-N') so nick colors can
;; be themed and customized like any other face, instead of being applied
;; as anonymous (:foreground ...) specs.

(defun clatter-hl--nick-face-name (idx)
  "Return the face symbol for palette index IDX."
  (intern (format "clatter-nick-color-%d" idx)))

(defun clatter-hl-rebuild-nick-faces (&optional force)
  "Define a named face for each color in `clatter-hl-nick-colors'.
Face `clatter-nick-color-N' uses the Nth palette color as a bold
foreground.  Existing faces are left alone unless FORCE is non-nil (as
when called interactively), so user or theme customizations are
preserved across reloads; call with a prefix arg to refresh them after
changing the palette."
  (interactive (list t))
  (let ((idx 0))
    (dolist (color clatter-hl-nick-colors)
      (let ((face (clatter-hl--nick-face-name idx)))
        (cond
         ;; New face: declare it so it is themeable and shows up in Customize.
         ((not (facep face))
          (custom-declare-face
           face `((t (:foreground ,color :weight bold)))
           (format "Clatter nick highlight color %d." idx)
           :group 'clatter))
         ;; Existing face: only overwrite when explicitly forced, so user or
         ;; theme customizations survive normal reloads.
         (force
          (set-face-attribute face nil :foreground color :weight 'bold))))
      (setq idx (1+ idx)))))

(defun clatter-hl-nick-face-symbol (nick)
  "Return the named face symbol used to highlight NICK.
A pal (see `clatter-pals') gets the `clatter-pal' face; otherwise the
deterministic, hash-based `clatter-nick-color-N' palette face."
  (if (clatter-pal-p nick)
      'clatter-pal
    (clatter-hl--nick-face-name (clatter-hl-nick-index nick))))

;; --- In-text highlighting ---

(defun clatter-hl-nicks-in-string (text buffer)
  "Return TEXT with nicks from BUFFER's nick list highlighted.
Only highlights nicks that are currently in the channel."
  (if (or (not clatter-hl-nicks-enabled)
          (not (buffer-live-p buffer)))
      text
    (with-current-buffer buffer
      (when clatter--nick-list
        (let ((nicks (clatter-hl--collect-nicks buffer)))
          (with-syntax-table clatter-nick-syntax-table
            (dolist (nick nicks)
              (let ((re (rx bow (literal nick) eow)))
                (setq text (clatter-hl--propertize-matches
                            text re
                            (list 'face (clatter-hl-nick-face-symbol nick)
                                  'clatter-nick nick))))))))
      text)))

(defun clatter-hl--collect-nicks (buffer)
  "Collect eligible nicks from BUFFER's nick list."
  (with-current-buffer buffer
    (let (nicks)
      (when clatter--nick-list
        (maphash (lambda (nick _prefix)
                   (when (and (>= (length nick) clatter-hl-nicks-minimum-length)
                              (not (member (downcase nick)
                                           (mapcar #'downcase
                                                   clatter-hl-nicks-skip-nicks))))
                     (push nick nicks)))
                 clatter--nick-list))
      nicks)))

(defun clatter-hl--propertize-matches (text regexp props)
  "Add PROPS to all matches of REGEXP in TEXT.
Skips regions that already have a face from `clatter-hl-nicks-skip-faces'."
  (let ((pos 0)
        (result ""))
    (while (string-match regexp text pos)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (existing-face (get-text-property start 'face text)))
        (setq result (concat result (substring text pos start)))
        (if (and existing-face
                 (or (memq existing-face clatter-hl-nicks-skip-faces)
                     (and (listp existing-face)
                          (cl-intersection existing-face
                                           clatter-hl-nicks-skip-faces))))
            ;; Skip - face is in the exclusion list
            (setq result (concat result (substring text start end)))
          ;; Apply nick highlight
          (setq result (concat result
                               (apply #'propertize
                                      (substring text start end)
                                      props))))
        (setq pos end)))
    (concat result (substring text pos))))

;; --- Highlight nicks in the <nick> prefix too ---

(defun clatter-hl-nick-face (nick conn)
  "Return the face used to highlight NICK on CONN.
Returns `clatter-my-nick' for our own nick, otherwise a named
`clatter-nick-color-N' face from the palette."
  (if (string-equal nick (clatter-connection-nick conn))
      'clatter-my-nick
    (clatter-hl-nick-face-symbol nick)))

;; --- URL detection and highlighting ---

(defvar clatter-url-regexp
  "https?://[^ \t\n\r<>\"']+"
  "Regexp matching URLs in IRC messages.")

(defun clatter-hl-urls-in-string (text)
  "Highlight URLs in TEXT as standard Emacs buttons."
  (let ((pos 0)
        (result ""))
    (while (string-match clatter-url-regexp text pos)
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (url (match-string 0 text)))
        (setq result (concat result (substring text pos start)))
        (let ((button-text
               (propertize url
                           'button '(t)
                           'category 'default-button
                           'follow-link t
                           'clatter-url url
                           'face '(:foreground "#89ddff" :underline t)
                           'help-echo url
                           'action (lambda (button)
                                     (browse-url (button-get button 'clatter-url))))))
          (setq result (concat result button-text)))
        (setq pos end)))
    (concat result (substring text pos))))

(defvar clatter-hl-url-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'clatter-hl-open-url-at-point)
    (define-key map [mouse-1] #'clatter-hl-open-url-at-point)
    map)
  "Keymap for clickable URLs in clatter buffers.")

(defun clatter-hl-open-url-at-point ()
  "Open the URL at point."
  (interactive)
  (let ((url (get-text-property (point) 'clatter-url)))
    (if url
        (browse-url url)
      (message "No URL at point"))))

(defun clatter-hl-open-url-nearest ()
  "Open the nearest URL on the current line, searching forward then backward."
  (interactive)
  (let ((url (get-text-property (point) 'clatter-url)))
    (if url
        (browse-url url)
      ;; Search forward on line
      (let ((found nil)
            (pos (point))
            (eol (line-end-position))
            (bol (line-beginning-position)))
        (save-excursion
          ;; Forward
          (while (and (not found) (< pos eol))
            (setq pos (next-single-property-change pos 'clatter-url nil eol))
            (when (and pos (get-text-property pos 'clatter-url))
              (setq found (get-text-property pos 'clatter-url))))
          ;; Backward
          (unless found
            (setq pos (point))
            (while (and (not found) (> pos bol))
              (setq pos (previous-single-property-change pos 'clatter-url nil bol))
              (when (and pos (get-text-property pos 'clatter-url))
                (setq found (get-text-property pos 'clatter-url))))))
        (if found
            (browse-url found)
          (message "No URL on this line"))))))

;; --- URL collector ---

(defun clatter-hl--collect-urls ()
  "Collect all URLs from the current buffer with context.
Returns a list of (DISPLAY-STRING . URL) pairs, newest first."
  (let (urls)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((url (get-text-property (point) 'clatter-url)))
          (when url
            (let* ((bol (line-beginning-position))
                   (sender (get-text-property bol 'clatter-sender))
                   (display (if sender
                                (format "%s - %s" sender url)
                              (format "%s" url))))
              (unless (cl-find url urls :key #'cdr :test #'string-equal)
                (push (cons display url) urls)))))
        ;; Jump to next url property change
        (let ((next (next-single-property-change (point) 'clatter-url)))
          (if next
              (goto-char next)
            (goto-char (point-max))))))
    urls))

(defun clatter-hl-browse-urls ()
  "Show all URLs from the current buffer and open the selected one."
  (interactive)
  (let ((urls (clatter-hl--collect-urls)))
    (if (null urls)
        (message "No URLs found in this buffer")
      (let* ((selection (completing-read "Open URL: " urls nil t))
             (url (cdr (assoc selection urls))))
        (when url
          (browse-url url))))))

(defun clatter-hl-copy-url ()
  "Show all URLs from the current buffer and copy the selected one."
  (interactive)
  (let ((urls (clatter-hl--collect-urls)))
    (if (null urls)
        (message "No URLs found in this buffer")
      (let* ((selection (completing-read "Copy URL: " urls nil t))
             (url (cdr (assoc selection urls))))
        (when url
          (kill-new url)
          (message "Copied: %s" url))))))

;; --- Integration: apply all highlighting to message text ---

(defun clatter-hl-format-text (text buffer conn)
  "Apply all highlighting to message TEXT for BUFFER using CONN.
Applies mIRC formatting first, then URLs, then nick highlighting."
  (ignore conn)
  (let ((result text))
    (setq result (clatter-format-parse result))
    (setq result (clatter-hl-urls-in-string result))
    (setq result (clatter-hl-nicks-in-string result buffer))
    (setq result (clatter-hl-keywords-in-string result))
    result))

;; Build the named nick faces once at load time.
(clatter-hl-rebuild-nick-faces)

(provide 'clatter-hl-nicks)

;;; clatter-hl-nicks.el ends here
