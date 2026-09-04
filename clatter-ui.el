;;; clatter-ui.el --- Buffer rendering, faces, and input -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; UI layer for clatter.el.  Renders messages into Emacs buffers,
;; defines faces for nick colorization, handles user input from the
;; prompt, and manages mode-line display.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'seq)
(require 'clatter-config)
(require 'clatter-format)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-model)
(require 'clatter-handlers)
(require 'clatter-hl-nicks)
(require 'clatter-smart)
(require 'clatter-pals)

;; --- Faces ---

(defvar-local clatter--last-timestamp-key nil
  "Coalescing key of the last timestamped message in this buffer.
The bucket key from `clatter--timestamp-bucket-key', shared by every
timestamp side.")

;; Clatter faces inherit from standard theme faces (font-lock-*,
;; error, success) rather than hardcoding hex colors, so they stay legible
;; across light and dark Emacs themes and adapt automatically on `load-theme'.

(defface clatter-timestamp
  '((t :inherit font-lock-doc-face))
  "Face for message timestamps."
  :group 'clatter)

(defface clatter-divider
  '((t :inherit shadow))
  "Face for divider rows."
  :group 'clatter)

(defface clatter-nick
  '((t :weight bold))
  "Default face for nicks."
  :group 'clatter)

(defface clatter-my-nick
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for your own nick."
  :group 'clatter)

(defface clatter-action
  '((t :inherit font-lock-string-face :slant italic))
  "Face for /me action messages."
  :group 'clatter)

(defface clatter-notice
  '((t :inherit font-lock-type-face))
  "Face for NOTICE messages."
  :group 'clatter)

(defface clatter-reaction
  '((t :inherit font-lock-type-face))
  "Face for reactions."
  :group 'clatter)

(defface clatter-system
  '((t :inherit font-lock-comment-face))
  "Face for system/status messages."
  :group 'clatter)

(defface clatter-error
  '((t :inherit error :weight bold))
  "Face for error messages."
  :group 'clatter)

(defface clatter-prompt
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the input prompt."
  :group 'clatter)

(defface clatter-mention
  '((t :inherit error :weight bold))
  "Face for highlighted mentions of your nick."
  :group 'clatter)

(defface clatter-channel
  '((t :inherit font-lock-builtin-face))
  "Face for channel names."
  :group 'clatter)

(defface clatter-muted-reaction
  '((t :strike-through t))
  "Face for muted reactions."
  :group 'clatter)

(defface clatter-bot-label-face
  '((t :inherit font-lock-preprocessor-face))
  "Face used for the bot label."
  :group 'clatter)

;; --- Bot label ---

(defcustom clatter-bot-label "[bot]"
  "Label added to messages to messages sent by bots."
  :type 'string
  :group 'clatter)

;; --- Away indicator ---

(defcustom clatter-away-indicator "@"
  "Indicator appended to nick to indicate we are away."
  :type '(choice (const :tag "No indicator" nil)
                 (string :tag "Indicator"))
  :group 'clatter)

(defcustom clatter-compact-system-messages nil
  "Control compact rendering of presence and moderation events.

When nil, render the existing verbose system messages.  The value
`compact' uses essential context and groups consecutive presence events
on one line.  The value `essential' shows only the identities and action
data needed to understand an event without grouping.  The value
`reasons' additionally shows PART, QUIT, KICK, and AWAY reasons.  The
value `full' also shows available channel, realname, invitee, and target
context."
  :type '(choice (const :tag "Verbose" nil)
                 (const :tag "Grouped compact events" compact)
                 (const :tag "Essential context" essential)
                 (const :tag "Essential context and reasons" reasons)
                 (const :tag "Full compact context" full))
  :group 'clatter)

(defcustom clatter-compact-system-group-window 180
  "Seconds in which consecutive compact events may share a line.
Only events with compatible visibility are grouped.  Any intervening
message ends the group."
  :type 'number
  :group 'clatter)

(defcustom clatter-compact-system-separator " · "
  "String inserted between events on a grouped compact system line."
  :type 'string
  :group 'clatter)

(defcustom clatter-compact-system-symbols
  '((join . "→")
    (part . "←")
    (quit . "×")
    (nick . "»")
    (away . "○")
    (back . "●")
    (mode . "±")
    (kick . "⬾")
    (invite . "✉"))
  "Alist mapping compact system event types to prefix symbols."
  :type '(alist :key-type symbol :value-type string)
  :group 'clatter)

(defcustom clatter-group-messages-by-nick nil
  "Collapse the nick column on consecutive messages from the same nick.
When non-nil, only the first message of a burst from a nick shows the
nick prefix; subsequent same-nick PRIVMSG lines within
`clatter-group-messages-window' seconds render a blank nick column.
A burst is broken by an intervening message from another nick, an
action, a notice, or a time gap larger than the window.  Only PRIVMSG
lines are grouped; actions and notices always show their prefix."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-group-messages-window 300
  "Seconds within which consecutive same-nick PRIVMSGs share one nick.
A gap larger than this breaks the burst.  nil means adjacency only:
any intervening line breaks the group but a time gap alone never does."
  :type '(choice (const :tag "No time limit (adjacency only)" nil)
                 (number :tag "Seconds"))
  :group 'clatter)

(defcustom clatter-group-messages-gap nil
  "Vertical space between nick groups when message grouping is enabled.
An integer specifies pixels; a float specifies a multiple of the default
frame line height.  nil or a non-positive value disables the gap.  Line
spacing has no effect on text terminals.

With an `oldest-first' bottom prompt, gaps make screen lines taller than
Emacs's line-granular scrolling can always divide evenly: the pinned
input stays fully visible but may float up to one line above the window
bottom while a short history still fills the window."
  :type '(choice (const :tag "No gap" nil)
                 (number :tag "Line spacing"))
  :group 'clatter)

;; --- Message insertion ---

(defvar-local clatter--prompt-marker nil
  "Marker for the start of the input prompt.")

(defvar-local clatter--input-marker nil
  "Marker for the start of user input (after prompt text).")

(defvar-local clatter--messages-marker nil
  "Marker for the start of the message area (below the input line).")

(defvar-local clatter--input-padding-end nil
  "Marker after protected oldest-first padding and before message history.")

(defvar-local clatter--typing-indicator-overlay nil
  "Overlay that renders typing status on the reserved separator row.")

(defun clatter--ensure-input-padding (lines)
  "Ensure oldest-first history has at least LINES real blank lines above it.
Real buffer lines avoid redisplay ambiguities caused by overlay display
strings.  They give each window enough scrollable space to place the input on
its final text row while short history grows upward."
  (when (and (eq clatter-message-order 'oldest-first)
             (markerp clatter--input-padding-end)
             (marker-buffer clatter--input-padding-end))
    (let* ((current (count-lines (point-min) clatter--input-padding-end))
           (missing (max 0 (- lines current))))
      (when (> missing 0)
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (save-excursion
            (goto-char clatter--input-padding-end)
            (insert (propertize
                     (make-string missing ?\n)
                     'read-only t
                     'front-sticky t
                     'rear-nonsticky t
                     'clatter-input-padding t))
            ;; This marker deliberately does not advance for ordinary message
            ;; insertion, so move it explicitly only when extending padding.
            (set-marker clatter--input-padding-end (point))))))))

(defun clatter--window-follows-input-p (window)
  "Return non-nil when WINDOW is displaying and following this input area."
  (and (window-live-p window)
       (eq (window-buffer window) (current-buffer))
       (clatter-in-input-p (window-point window))))

(defun clatter--pixel-pin-start (window)
  "Return the highest window start keeping the input fully visible in WINDOW.
Walk back one screen line at a time from the input, measuring the real
pixel height of each candidate range, and stop just before the range
would exceed the window body.  Moves point; callers wrap in
`save-excursion'."
  (let* ((end (clatter--input-end))
         (body (window-body-height window t))
         (cand (progn (goto-char end)
                      (vertical-motion 0 window)
                      (point)))
         (best cand)
         (guard 0))
    ;; ponytail: linear walk, one pixel measurement per screen line,
    ;; bounded to 400 lines; plenty for any real window height.
    (while (and (> cand (point-min)) (< guard 400))
      (cl-incf guard)
      (goto-char cand)
      (vertical-motion -1 window)
      (let ((prev (point)))
        (if (or (>= prev cand)
                (> (cdr (window-text-pixel-size window prev end)) body))
            (setq cand (point-min))     ; top reached or next line overflows
          (setq best prev
                cand prev))))
    best))

(defun clatter--recenter-input-window (window)
  "Put the final input line at the bottom of WINDOW without moving point."
  (let ((position (window-point window))
        start)
    (save-selected-window
      (with-selected-window window
        (save-excursion
          ;; `recenter' walks back with the frame's default line metrics,
          ;; so `line-spacing' gaps (`clatter-group-messages-gap') make it
          ;; over- or undershoot by up to one gap per boundary: the input
          ;; line jitters and can clip off the window bottom as messages
          ;; arrive.  With gaps enabled, compute the start from measured
          ;; pixel heights instead; the input then never clips, at the
          ;; cost of floating below the last line by less than one line
          ;; when the heights do not divide the window evenly.
          (if (and clatter-group-messages-by-nick
                   (numberp clatter-group-messages-gap)
                   (> clatter-group-messages-gap 0))
              (setq start (clatter--pixel-pin-start window))
            (goto-char (clatter--input-end))
            (recenter -1)
            (setq start (window-start window))))))
    (when (window-live-p window)
      (set-window-point window position)
      ;; Restoring point can make redisplay choose a different start.  Reapply
      ;; the start calculated with input at the bottom and request it exactly.
      (when start
        (set-window-start window start t)))))

(defun clatter--input-fits-in-window-p (window)
  "Return non-nil when oldest-first history and input fit in WINDOW.
Only scan as many screen lines as WINDOW can display so this check stays
constant-time as message history grows."
  (save-excursion
    (goto-char clatter--input-padding-end)
    (vertical-motion (window-body-height window) window)
    (eobp)))

(defun clatter--pin-input-in-window (window)
  "Keep oldest-first input on the bottom line of following WINDOW."
  (when (or (clatter--window-follows-input-p window)
            (clatter--input-fits-in-window-p window))
    (clatter--recenter-input-window window)))

(defun clatter--refresh-input-spacers (&optional buffer)
  "Refresh bottom-pinned input display for BUFFER's visible windows.
Short histories remain stacked immediately above the input regardless of
point.  Once history exceeds a window, only a window whose point remains in
the input area follows new messages; a scrolled history view retains its
viewport."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((windows (get-buffer-window-list buffer nil 'visible)))
          (if (and (eq clatter-message-order 'oldest-first)
                   clatter--prompt-marker
                   clatter--input-marker
                   clatter--input-padding-end)
              (progn
                (clatter--ensure-input-padding
                 (max 0 (- (apply #'max 1
                                  (mapcar #'window-body-height windows))
                           (if (overlayp clatter--typing-indicator-overlay)
                               2
                             1))))
                (dolist (window windows)
                  (clatter--pin-input-in-window window)))))))))

(defun clatter-in-input-p (&optional position)
  "Return non-nil when POSITION is in the input area.
POSITION defaults to point."
  (let ((position (or position (point))))
    (and clatter--input-marker
         clatter--messages-marker
         (<= (marker-position clatter--input-marker) position)
         (<= position (clatter--input-end)))))

(defun clatter--navigation-property-positions (property)
  "Return visible starts of non-nil PROPERTY regions in this buffer."
  (let ((position (point-min))
        positions)
    (while (< position (point-max))
      (when (and (get-text-property position property)
                 (not (invisible-p position)))
        (push position positions))
      (setq position
            (or (next-single-property-change
                 position property nil (point-max))
                (point-max))))
    (nreverse positions)))

(defun clatter--navigation-button-positions ()
  "Return visible starts of standard buttons in this buffer."
  (let ((position (point-min))
        button
        positions)
    (while (and (< position (point-max))
                (setq button (next-button position t)))
      (let ((start (button-start button))
            (end (button-end button)))
        (when (and start (not (invisible-p start)))
          (push start positions))
        (setq position (max (1+ start) end))))
    (nreverse positions)))

(defun clatter--navigation-positions ()
  "Return sorted visible navigation target positions in this buffer."
  (sort (delete-dups
         (append
          (clatter--navigation-property-positions
           'clatter-navigation-target)
          (clatter--navigation-button-positions)
          ;; Include existing non-button interactive text so extensions do
          ;; not need to know about Clatter's private target property.
          (clatter--navigation-property-positions 'follow-link)
          (clatter--navigation-property-positions 'keymap)
          (clatter--navigation-property-positions 'clatter-url)
          (when (and clatter--input-marker
                     (marker-buffer clatter--input-marker))
            (list (marker-position clatter--input-marker)))))
        #'<))

(defun clatter-next-item ()
  "Move point to the next visible message or interactive item."
  (interactive)
  (let ((position (seq-find (lambda (candidate)
                              (> candidate (point)))
                            (clatter--navigation-positions))))
    (if position
        (goto-char position)
      (user-error "No next Clatter item"))))

(defun clatter-previous-item ()
  "Move point to the previous visible message or interactive item."
  (interactive)
  (let ((position (seq-find (lambda (candidate)
                              (< candidate (point)))
                            (reverse (clatter--navigation-positions)))))
    (if position
        (goto-char position)
      (user-error "No previous Clatter item"))))

(defun clatter-tab ()
  "Complete at point in input, or move to the next history item."
  (interactive)
  (if (clatter-in-input-p)
      (completion-at-point)
    (clatter-next-item)))

(defun clatter-backtab ()
  "Keep BACKTAB undefined in input, or move to the previous history item."
  (interactive)
  (if (clatter-in-input-p)
      (call-interactively #'undefined)
    (clatter-previous-item)))

(defvar-local clatter--message-generation 0
  "Number of messages inserted into the current buffer.")

(defvar-local clatter--compact-system-group nil
  "Metadata for the most recent grouped compact system line.")

(defvar clatter--compact-system-group-id 0
  "Monotonic identifier for compact system group lines.")

(defun clatter--compact-system-now ()
  "Return the current time in seconds for compact event grouping."
  (float-time))

(defvar-local clatter--prompt-shows-nick nil
  "Non-nil when the current prompt already displays the connection nick.")

(defvar-local clatter--pending-self-echoes nil
  "Tentative outgoing messages awaiting their server echoes.")

(defvar clatter--self-echo-nonce 0
  "Monotonically increasing identifier for tentative self echoes.")

(defun clatter--fool-invisibility-p (invisible)
  "Return non-nil if INVISIBLE includes the fool visibility category."
  (or (eq invisible 'clatter-fool)
      (and (listp invisible)
           (memq 'clatter-fool invisible))))

(defun clatter--nick-column-display-width (s)
  "Return the display width in columns S occupies in the nick column.
The ellipsis `…' (U+2026) is counted as 2 columns: many terminal fonts
render it double-width even though Emacs's `char-width' reports 1.
Padding by this width keeps a truncated nick's closing delimiter lined
up with the nicks above and below it instead of shifted one column right.
Other characters use their `char-width'."
  (let ((w 0))
    (dolist (c (string-to-list s))
      (setq w (+ w (if (= c ?…) 2 (char-width c)))))
    w))

(defun clatter--format-sender (sender)
  "Render SENDER through `clatter-sender-format', replacing `%nick'."
  (replace-regexp-in-string "%nick" sender clatter-sender-format t t))

(defun clatter--truncate-nick-column (nick-str width)
  "Truncate NICK-STR for the nick column, fitting within WIDTH display cols.
A nick wrapped by `clatter-sender-format', or a `-nick-' notice form,
keeps its delimiters and signals the cut with an ellipsis (`…'), so a
long nick becomes e.g. `<longnic…>'.  The body is trimmed so the
result's display width (counting `…' as two cells, see
`clatter--nick-column-display-width') is WIDTH, which makes the closing
delimiter line up with the delimiters of shorter nicks in fonts that
render the ellipsis wide.  Any other shape is clipped to WIDTH display
cols."
  (save-match-data
    (let* ((parts (split-string clatter-sender-format "%nick"))
           (pre (or (car parts) ""))
           (post (or (cadr parts) ""))
           ;; delimiters + the double-width ellipsis
           (overhead (+ (length pre) (length post) 2)))
      (cond
       ((and (> width overhead)
             (string-prefix-p pre nick-str)
             (string-suffix-p post nick-str)
             (> (length nick-str) (+ (length pre) (length post))))
        (concat pre
                (substring nick-str (length pre)
                           (+ (length pre) (- width overhead)))
                "…" post))
       ((and (>= width 5)
             (string-match "\\`-\\(.+\\)-\\'" nick-str))
        (concat "-" (substring (match-string 1 nick-str) 0 (- width 4)) "…-"))
       (t (substring nick-str 0 (1- width)))))))

(defun clatter--format-nick-column (nick-str &optional face sender)
  "Right-align NICK-STR within `clatter-nick-column-width'.
Apply FACE and set clatter-sender property to SENDER if provided.

When `clatter-nick-column-truncate' is non-nil, NICK-STR longer than
the column width is truncated to fit (preserving the `<...>' or
`-...-' delimiters with an ellipsis), so a very long nick does not
push its message text past the column and break alignment with
neighbors."
  (let* ((width clatter-nick-column-width)
         (nick-text (copy-sequence nick-str))
         padded)
    (when (and clatter-nick-column-truncate
               (> (clatter--nick-column-display-width nick-text) width))
      (setq nick-text (clatter--truncate-nick-column nick-text width)))
    (let ((pad (max 0 (- width
                         (clatter--nick-column-display-width nick-text)))))
      (when face
        (add-face-text-property 0 (length nick-text) face nil nick-text))
      (add-text-properties 0 (length nick-text)
                           '(clatter-navigation-target message)
                           nick-text)
      (setq padded (concat (make-string pad ?\s) nick-text))
      (when sender
        (setq padded (propertize padded 'clatter-sender sender)))
      (propertize padded 'clatter-nick-column t))))

(defun clatter--format-system-prefix (prefix-str)
  "Right-align PREFIX-STR (e.g. \"***\") within the nick column."
  (let* ((width clatter-nick-column-width)
         (plen (length prefix-str))
         (pad (max 0 (- width plen))))
    (propertize
     (concat (make-string pad ?\s)
             (propertize prefix-str
                         'face 'clatter-system
                         'clatter-navigation-target 'message))
     'clatter-nick-column t)))

(defvar clatter--group-messages-adjacency-only nil
  "When non-nil, `clatter--group-with-previous-p' ignores the time window.
Bound around history/batch playback, which replays messages with their
original (often widely spaced) timestamps: visual adjacency is the right
grouping signal there, not the live burst window.")

(defvar clatter--insert-at-backlog-end nil
  "When non-nil, messages insert at the buffer's oldest end.
Bound while rendering a backwards CHATHISTORY page (see
`clatter-chathistory-more') so an older page lands past the existing
history instead of next to the prompt.")

(defun clatter--backlog-insert-position ()
  "Return the position of the current buffer's oldest end.
For `newest-first' that is `point-max'; for `oldest-first' it is the end
of the protected input padding at the top of the message area."
  (if (eq clatter-message-order 'oldest-first)
      (or (and (markerp clatter--input-padding-end)
               (marker-position clatter--input-padding-end))
          (point-min))
    (point-max)))

(defun clatter--message-insert-position ()
  "Return the position where the next message inserts in the current buffer."
  (if clatter--insert-at-backlog-end
      (clatter--backlog-insert-position)
    (or (and (markerp clatter--messages-marker)
             (marker-position clatter--messages-marker))
        (point-max))))

(defun clatter--previous-message-bol (&optional buffer)
  "Return the bol of the chronologically-previous message in BUFFER.
BUFFER defaults to the current buffer.  The neighbor is the message
adjacent to where the next message will insert (see
`clatter--message-insert-position').  Inserts that push earlier messages
away leave the neighbor on the line at that position; inserts that append
after them leave it on the line above.  Lines currently hidden by
`buffer-invisibility-spec' (fools, mutes) do not anchor visual grouping
and are skipped over.  Returns nil when the position is out of range or
no message text properties are present there."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((pos (clatter--message-insert-position)))
          (save-excursion
            (goto-char pos)
            ;; Backlog rendering reverses which side the previous message
            ;; sits on: newest-first appends, oldest-first prepends.
            (let ((backward (eq (and clatter--insert-at-backlog-end t)
                                (eq clatter-message-order 'newest-first)))
                  (guard 0))
              (when backward (forward-line -1))
              ;; Hidden neighbors would bake grouping decisions against
              ;; text the user cannot see: continue to the nearest visible
              ;; line in the same direction.
              ;; ponytail: linear scan capped at 500 hidden lines; a longer
              ;; hidden run just disables grouping for that message.
              (while (and (< guard 500)
                          (invisible-p (line-beginning-position))
                          (if backward
                              (> (line-beginning-position) (point-min))
                            (< (line-end-position) (point-max))))
                (cl-incf guard)
                (forward-line (if backward -1 1))))
            (let ((bol (line-beginning-position)))
              (and (not (invisible-p bol))
                   (get-text-property bol 'clatter-sender)
                   bol))))))))

(defun clatter--group-with-message-p (bol sender server-time)
  "Return non-nil when SENDER groups with the message at BOL.
The caller must arrange for BOL to be the previous visible message."
  (and bol
       (eq (get-text-property bol 'clatter-msg-type) 'privmsg)
       (let ((prev-sender (get-text-property bol 'clatter-sender)))
         (and prev-sender
              (string-equal-ignore-case prev-sender sender)
              (let ((prev-time
                     (get-text-property bol 'clatter-server-time))
                    (new-time (or server-time (current-time))))
                (or (null clatter-group-messages-window)
                    clatter--group-messages-adjacency-only
                    (null prev-time)
                    (null new-time)
                    (<= (abs
                         (float-time
                          (time-subtract new-time prev-time)))
                        clatter-group-messages-window)))))))

(defun clatter--group-with-previous-p (buffer sender server-time)
  "Return non-nil when SENDER's new message groups with the previous one.
This is true when `clatter-group-messages-by-nick' is enabled, the
chronologically-previous visible message in BUFFER is a PRIVMSG from
SENDER (case-insensitive), and the gap between the two is within
`clatter-group-messages-window'.  When either timestamp is missing the
window check is skipped (adjacency alone groups), as it is when
`clatter--group-messages-adjacency-only' is set (e.g. during history
playback).  SERVER-TIME is the Emacs time value of the new message; nil
means now."
  (and clatter-group-messages-by-nick
       (with-current-buffer (or buffer (current-buffer))
         (clatter--group-with-message-p
          (clatter--previous-message-bol) sender server-time))))

(defun clatter--refresh-message-groups ()
  "Recompute visible nick groups and gaps in the current buffer."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        positions)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (when (get-text-property (point) 'clatter-nick-column)
          (push (point) positions))
        (forward-line 1)))
    (dolist (bol positions)
      (let* ((line-end (save-excursion
                         (goto-char bol)
                         (line-end-position)))
             (end (next-single-property-change
                   bol 'clatter-nick-column nil line-end)))
        (remove-text-properties
         bol end '(clatter-grouped nil display nil)))
      (let ((eol (save-excursion (goto-char bol) (line-end-position))))
        (when (< eol (point-max))
          (remove-text-properties eol (1+ eol) '(line-spacing nil)))))
    (when clatter-group-messages-by-nick
      (when (eq clatter-message-order 'oldest-first)
        (setq positions (nreverse positions)))
      (let (previous)
        (dolist (bol positions)
          (unless (invisible-p bol)
            (let* ((sender (get-text-property bol 'clatter-sender))
                   (server-time
                    (get-text-property bol 'clatter-server-time))
                   (grouped-p
                    (and sender
                         (eq (get-text-property bol 'clatter-msg-type)
                             'privmsg)
                         (clatter--group-with-message-p
                          previous sender server-time))))
              (when grouped-p
                (let* ((line-end (save-excursion
                                   (goto-char bol)
                                   (line-end-position)))
                       (end (next-single-property-change
                             bol 'clatter-nick-column nil line-end)))
                  (add-text-properties
                   bol end
                   (list 'clatter-grouped t
                         'display
                         (make-string clatter-nick-column-width ?\s)))))
              (when (and previous
                         (not grouped-p)
                         (numberp clatter-group-messages-gap)
                         (> clatter-group-messages-gap 0))
                (let* ((boundary
                        (if (eq clatter-message-order 'oldest-first)
                            previous bol))
                       (eol (save-excursion
                              (goto-char boundary)
                              (line-end-position))))
                  (when (< eol (point-max))
                    (put-text-property
                     eol (1+ eol) 'line-spacing
                     clatter-group-messages-gap))))
              (setq previous bol))))))))

(defun clatter--update-undo-list (shift)
  "Shift integer buffer positions in `buffer-undo-list' by SHIFT.
Called after inserting messages above the input line (the bottom,
oldest-first prompt) so the user's pending undo entries keep pointing at
their input text instead of the freshly inserted message.  Based on the
approach in `erc-update-undo-list'."
  (unless (or (zerop shift) (atom buffer-undo-list))
    (let ((list buffer-undo-list) elt)
      (while list
        (setq elt (car list))
        (cond
         ((integerp elt)                ; POSITION
          (setcar list (+ elt shift)))
         ((or (atom elt)                ; nil boundary, (t . TIME)
              (markerp (car elt)))      ; (MARKER . DISTANCE) - auto-adjusted
          nil)
         ((integerp (car elt))          ; (BEGIN . END)
          (setcar elt (+ (car elt) shift))
          (setcdr elt (+ (cdr elt) shift)))
         ((stringp (car elt))           ; (TEXT . POSITION)
          (setcdr elt (+ (cdr elt)
                         (* (if (natnump (cdr elt)) 1 -1) shift))))
         ((null (car elt))              ; (nil PROPERTY VALUE BEG . END)
          (let ((cons (nthcdr 3 elt)))
            (setcar cons (+ (car cons) shift))
            (setcdr cons (+ (cdr cons) shift)))))
        (setq list (cdr list))))))

(defun clatter--body-width (&optional buffer)
  "Return the body width of a window showing BUFFER, else the frame width.
The frame fallback keeps headless and temp-buffer contexts working."
  (if-let* ((win (car (get-buffer-window-list (or buffer (current-buffer))
                                              nil 'visible))))
      (window-body-width win)
    (frame-width)))

(defun clatter--effective-fill-column (&optional buffer)
  "Return the column at which to hard-wrap inserted message text.
When `clatter-fill-column' is an integer, return it.  When it is the
symbol `auto', return the body width of the first window displaying
BUFFER (default: the current buffer), floored one column past the nick
indent and capped at `clatter-max-line-length'.  With no live window,
fall back to the selected frame width so headless and temp-buffer
contexts still wrap.  `window-body-width' already excludes the timestamp
margin gutter, so no further margin subtraction is needed.  Return nil
when wrapping is disabled (`clatter-fill-column' is nil) or the derived
column is too narrow to wrap past the nick indent."
  (let ((floor-col (1+ clatter-nick-column-width)))
    (pcase clatter-fill-column
      ('nil nil)
      ('auto
       (let ((col (min (clatter--body-width buffer)
                       (or clatter-max-line-length 400))))
         (when (> col floor-col) col)))
      (col (when (and (integerp col) (> col floor-col)) col)))))

(defun clatter--timestamp-margin-p ()
  "Return non-nil when timestamps use a window margin."
  (memq clatter-timestamp-side '(left right)))

(defun clatter--timestamp-overlay-p ()
  "Return non-nil when timestamps use a per-message overlay."
  (memq clatter-timestamp-side '(left right inline)))

;; `:align-to' runs before word-wrap, so leftover space on the last
;; visual row can't be used.  Decided at insert time — stale after a
;; window resize until the line is rewritten.
(defun clatter--timestamp-inline-break-p (eol ts-str)
  "Return non-nil when the line ending at EOL leaves no room for TS-STR."
  (save-excursion
    (goto-char eol)
    (> (current-column)
       (- (clatter--body-width) (string-width ts-str) 1))))

(defun clatter--timestamp-overlay-apply (ov ts-str &optional tooltip)
  "Attach the timestamp string for TS-STR to OV for the current side.
TOOLTIP, when non-nil, becomes the stamp's `help-echo'."
  (let ((stamp (propertize ts-str
                           'face '(clatter-timestamp default)
                           'help-echo tooltip)))
    (overlay-put ov 'help-echo tooltip)
    (if (eq clatter-timestamp-side 'inline)
        ;; Stamp sits on the message's last row via after-string at the
        ;; last text char — not the newline, whose before-string wraps
        ;; onto the next buffer line.  A stamp that doesn't fit is
        ;; dropped.  Keep the raw stamp on the overlay so
        ;; `clatter--timestamp-inline-refresh-at' can re-fit.
        (progn
          (overlay-put ov 'clatter-timestamp-str ts-str)
          (overlay-put ov 'clatter-timestamp-tooltip tooltip)
          (overlay-put ov 'before-string nil)
          (overlay-put ov 'after-string
                       (unless (clatter--timestamp-inline-break-p
                                (overlay-end ov) ts-str)
                         (concat (propertize
                                  " " 'display
                                  `(space :align-to
                                     (- right ,(string-width ts-str))))
                                 stamp))))
      ;; Apply 'default face after 'clatter-timestamp so no unwanted face
      ;; properties are inherited from text which might be at point.
      (overlay-put ov 'after-string nil)
      (overlay-put ov 'before-string
                   (propertize " " 'display
                               `((margin ,(if (eq clatter-timestamp-side 'left)
                                              'left-margin
                                            'right-margin))
                                 ,stamp))))))

(defun clatter--timestamp-inline-refresh-at (eol)
  "Re-fit the inline stamp on the line ending at EOL.
Compact system groups change a line after its stamp's fit was decided —
appends lengthen it, visibility toggles change its displayed width — so
apply the stamp again: the fit check drops or restores it, the same
rule as at insert time."
  (when (eq clatter-timestamp-side 'inline)
    (dolist (ov (overlays-in (max (point-min) (1- eol))
                             (min (point-max) (1+ eol))))
      (when (overlay-get ov 'clatter-timestamp)
        (when-let* ((ts-str (overlay-get ov 'clatter-timestamp-str)))
          (clatter--timestamp-overlay-apply
           ov ts-str (overlay-get ov 'clatter-timestamp-tooltip)))))))

(defun clatter--timestamp-bucket-key (time)
  "Return the coalescing key for TIME, shared by every timestamp side.
At `clatter-timestamp-interval' 1 this is the formatted timestamp, so
marks follow the displayed value.  Above 1, minutes are floored to the
interval: a value of 10 yields one mark per ten-minute clock bucket."
  (if (<= clatter-timestamp-interval 1)
      (format-time-string clatter-timestamp-format time)
    (let* ((dec (decode-time time))
           (minute (+ (* 60 (decoded-time-hour dec))
                      (decoded-time-minute dec))))
      (setf (decoded-time-second dec) 0
            (decoded-time-hour dec) 0
            (decoded-time-minute dec)
            (* clatter-timestamp-interval
               (/ minute clatter-timestamp-interval)))
      (format-time-string clatter-timestamp-format (encode-time dec)))))

(defun clatter--timestamp-divider-insert-row (formatted tooltip line-props
                                                        &optional invisible)
  "Insert a minute-divider row for FORMATTED time at point.
TOOLTIP becomes the row's help-echo; LINE-PROPS cover the whole row.
INVISIBLE carries the opening line's categories, so the row hides and
shows with the line that opened it, like a margin stamp would."
  (let ((start (point)))
    (insert (propertize (format "— %s —" formatted)
                        'face 'clatter-timestamp
                        'clatter-timestamp-divider t
                        'help-echo tooltip)
            "\n")
    (add-text-properties start (point) line-props)
    (put-text-property start (point) 'invisible invisible)))

(defun clatter--timestamp-divider-seed (buffer)
  "Insert an opening minute-divider row in BUFFER at the current time.
Without it a quiet buffer shows no time at all until the first spoken
message.  Seeds `clatter--last-timestamp-key' so a message arriving in
the same bucket does not open a second, adjacent row."
  (when (and (eq clatter-timestamp-side 'divider)
             clatter-timestamp-format)
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (buffer-undo-list t)
             (time (current-time))
             (formatted (format-time-string clatter-timestamp-format time))
             (tooltip (and clatter-timestamp-tooltip-format
                           (format-time-string
                            clatter-timestamp-tooltip-format time))))
        (save-excursion
          (goto-char (clatter--message-insert-position))
          (clatter--timestamp-divider-insert-row
           formatted tooltip
           (list 'read-only t
                 'front-sticky t
                 'wrap-prefix (make-string (1+ clatter-nick-column-width) ?\s)
                 'line-prefix "")))
        (setq clatter--last-timestamp-key
              (clatter--timestamp-bucket-key time))))))

(defun clatter--insert-message (buffer text &optional no-timestamp msg-props time invisible message-line-spacing)
  "Insert formatted TEXT into BUFFER.
Adds timestamp unless NO-TIMESTAMP is non-nil.
MSG-PROPS is an optional plist of extra text properties for the message line.
TIME is an optional Emacs time value (from IRCv3 server-time) for the timestamp.
MESSAGE-LINE-SPACING sets spacing below the message's final display line.
When `clatter-message-order' is `newest-first', messages appear directly below
the input line with older ones scrolling down.  When `oldest-first', messages
append at the bottom like a traditional IRC client."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (cl-incf clatter--message-generation)
      (let ((pre-input (and clatter--input-marker
                            (marker-position clatter--input-marker))))
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (save-excursion
            ;; Messages always insert at the messages marker.  Its position
            ;; and insertion type (set in `clatter--setup-prompt') determine
            ;; the layout: below a top prompt for `newest-first', or above a
            ;; bottom prompt for `oldest-first'.
            (goto-char (clatter--message-insert-position))
            (let* ((formatted-timestamp
                    (unless no-timestamp
                      (format-time-string clatter-timestamp-format time)))
                   (ts-key (and formatted-timestamp
                                (clatter--timestamp-bucket-key time)))
                   (ts-changed-p
                    (not (equal ts-key clatter--last-timestamp-key)))
                   (ts-str (and formatted-timestamp
                                (clatter--timestamp-overlay-p)
                                (or (not clatter-timestamp-only-if-changed)
                                    ts-changed-p)
                                formatted-timestamp))
                   (ts-tooltip-str
                    (and (not no-timestamp)
                         clatter-timestamp-tooltip-format
                         (format-time-string clatter-timestamp-tooltip-format time)))
                   (wrap-col (1+ clatter-nick-column-width))
                   (wrap-prefix (make-string wrap-col ?\s))
                   (line-props (list 'read-only t
                                     'front-sticky t
                                     'wrap-prefix wrap-prefix
                                     'line-prefix ""))
                   start)
              (when ts-key
                (setq clatter--last-timestamp-key ts-key))
              (when (and (eq clatter-timestamp-side 'divider)
                         ts-key ts-changed-p)
                (clatter--timestamp-divider-insert-row
                 formatted-timestamp ts-tooltip-str line-props invisible))
              ;; Newest-first inserts stay below their existing bucket row.
              (when (and (eq clatter-timestamp-side 'divider)
                         ts-key (not ts-changed-p)
                         (eq clatter-message-order 'newest-first)
                         (get-text-property (point)
                                            'clatter-timestamp-divider))
                (forward-line 1))
              (setq start (point))
              (insert text "\n")
              (when message-line-spacing
                (put-text-property (1- (point)) (point)
                                   'line-spacing message-line-spacing))
              (when-let* ((eff-col (clatter--effective-fill-column buffer)))
                (when (> eff-col wrap-col)
                  (let ((fill-column eff-col)
                        (fill-prefix wrap-prefix)
                        (adaptive-fill-mode nil))
                    (fill-region start (1- (point))))))
              (when ts-str
                (let* ((eol (1- (point)))
                       (ov (if (eq clatter-timestamp-side 'inline)
                               ;; Last text char, not the newline: after-string
                               ;; at overlay-end stays on this line.  Rear
                               ;; advances so compact appends keep the stamp
                               ;; at EOL without covering the next message.
                               (and (> eol start)
                                    (make-overlay (1- eol) eol nil t t))
                             (make-overlay start (1+ start) nil t))))
                  (when ov
                    (clatter--timestamp-overlay-apply ov ts-str ts-tooltip-str)
                    (overlay-put ov 'clatter-timestamp t)
                    (overlay-put ov 'invisible invisible))))
              (add-text-properties start (point) line-props)
              (when msg-props
                (add-text-properties start (point) msg-props))
              (when (clatter--fool-invisibility-p invisible)
                (add-face-text-property start (point) 'clatter-fool))
              (put-text-property start (point) 'invisible invisible)))
          (clatter--maybe-truncate buffer))
        ;; Messages inserted above the input (bottom/oldest-first prompt)
        ;; push the input down.  Without this, the user's pending undo
        ;; entries would still point at the old positions and an undo
        ;; would corrupt the freshly inserted message instead of their
        ;; input.  Shift those entries by the input's drift.
        (when (and pre-input clatter--input-marker)
          (clatter--update-undo-list
           (- (marker-position clatter--input-marker) pre-input)))
        (clatter--flyspell-prune-changes)
        (clatter--refresh-input-spacers buffer)))))

(defun clatter--flyspell-prune-changes ()
  "Drop flyspell change records that lie outside the editable input area.
Flyspell's after-change hook records a cons for every buffer
modification, but `flyspell-post-command-hook' only drains them when
the user runs a command in the buffer.  Message insertion happens from
process filters and timers, so in background buffers the records
accumulate without bound (hundreds of MB over hours on busy channels).
Only changes touching the input area can ever produce a spell check
(see `clatter--flyspell-predicate'); everything else is dead weight."
  (when (and (bound-and-true-p flyspell-mode)
             (boundp 'flyspell-changes)
             flyspell-changes
             clatter--input-marker
             (marker-buffer clatter--input-marker))
    (let ((beg (marker-position clatter--input-marker))
          (end (clatter--input-end)))
      (setq flyspell-changes
            (cl-delete-if (lambda (change)
                            (not (and (consp change)
                                      (integerp (car change))
                                      (integerp (cdr change))
                                      (<= (car change) end)
                                      (>= (cdr change) beg))))
                          flyspell-changes)))))

(defun clatter--maybe-truncate (_buffer)
  "Truncate the current buffer if it exceeds `clatter-buffer-max-lines'.
Removes oldest messages from the appropriate end of the buffer."
  (let* ((oldest-first-p (eq clatter-message-order 'oldest-first))
         (history-start (if oldest-first-p
                            (or (and clatter--input-padding-end
                                     (marker-position clatter--input-padding-end))
                                (point-min))
                          (or (and clatter--messages-marker
                                   (marker-position clatter--messages-marker))
                              (point-min))))
         (history-end (if oldest-first-p
                          (or (and clatter--messages-marker
                                   (marker-position clatter--messages-marker))
                              (point-max))
                        (point-max)))
         (line-count (count-lines history-start history-end)))
    (when (and clatter-buffer-max-lines
               (> line-count clatter-buffer-max-lines))
      (let ((inhibit-read-only t)
            (target-lines (- line-count
                             clatter-buffer-max-lines)))
        (save-excursion
          (if oldest-first-p
              ;; Preserve protected padding; delete from real history.
              (progn
                (goto-char history-start)
                (forward-line target-lines)
                (dolist (ov (overlays-in history-start (point)))
                  (delete-overlay ov))
                (delete-region history-start (point)))
            ;; Top prompt (newest-first): oldest messages are at the bottom.
            (goto-char history-end)
            (forward-line (- target-lines))
            (dolist (ov (overlays-in (point) history-end))
              (delete-overlay ov))
            (delete-region (point) history-end)))))))

(defun clatter--find-message-position-by-msgid (buffer msgid)
  "Find position of message in BUFFER identified by MSGID."
  (when (and (buffer-live-p buffer) msgid)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (when (string= msgid (get-text-property (point) 'clatter-msgid))
              (setq found (point)))
            (forward-line 1))
          found)))))

(defun clatter--find-message-by-msgid (buffer msgid)
  "Find message text and sender in BUFFER by MSGID.
Returns ((sender . text) . msg-type) or nil."
  (let ((found (clatter--find-message-position-by-msgid buffer msgid)))
    (when found
      (with-current-buffer buffer
        (cons (cons (get-text-property found 'clatter-sender)
                    (get-text-property found 'clatter-text))
              (get-text-property found 'clatter-msg-type))))))

(defun clatter-jump-to-msgid (buffer msgid)
  "Jump to BUFFER message identified by MSGID."
  (let ((found (clatter--find-message-position-by-msgid buffer msgid)))
    (when found
      (goto-char found))))

(defvar clatter--suppress-image-scan nil
  "When non-nil, `clatter-insert-privmsg' skips inline image scanning.
Bound around history/batch playback so a reconnect backlog does not scan
and fetch images for hundreds of old messages at once.")

(defun clatter-insert-generic (msg-type buffer sender text conn &optional server-time invisible extra-props)
  "Insert a MSG-TYPE from SENDER with TEXT into BUFFER using CONN context.
SERVER-TIME overrides the current time for the timestamp.  EXTRA-PROPS is
an optional plist appended to the text properties otherwise passed to
`clatter--insert-message' (used by `clatter-feed' to stash the source
network/target for jump-back)."
  (let* ((nick-face (clatter-hl-nick-face sender conn))
         (my-nick (clatter-connection-nick conn))
         (is-reply-to-me (get-text-property 0 'clatter-reply-to-me text))
         (is-mention (and my-nick
                          ;; do not highlight self-mentions
                          (not (string-equal-ignore-case sender my-nick))
                          (or is-reply-to-me
                              (clatter-mention-p (downcase my-nick) (downcase text)))))
         (reply-to (get-text-property 0 'clatter-reply-to text))
         (msgid (get-text-property 0 'clatter-msgid text))
         (self-echo-nonce (get-text-property 0 'clatter-self-echo-nonce text))
         (reply-context (when reply-to
                          (clatter--find-message-by-msgid buffer reply-to)))
         (hl-text (clatter-hl-format-text text buffer conn))
         (bot-tag (if (get-text-property 0 'clatter-bot sender)
                      (propertize clatter-bot-label 'face 'clatter-bot-label-face) ""))
         (bot-tag-delim (if (string-empty-p bot-tag) "" " "))
         ;; Only PRIVMSG lines can be grouped onto the previous message's
         ;; nick; actions and notices always show their own prefix and
         ;; break any open burst.
         (grouped-p (and (eq msg-type 'privmsg)
                         (clatter--group-with-previous-p buffer sender server-time)))
         (group-gap-p
          (and clatter-group-messages-by-nick
               (numberp clatter-group-messages-gap)
               (> clatter-group-messages-gap 0)
               (not grouped-p)
               ;; A currently-hidden message (fool, mute) renders nothing;
               ;; its gap would just space out the visible neighbors.
               (not (with-current-buffer buffer (invisible-p invisible)))
               (clatter--previous-message-bol buffer)))
         ;; The newline below the boundary depends on which side of the
         ;; previous message receives the new one.
         (insert-before-previous-p
          (not (eq (and clatter--insert-at-backlog-end t)
                   (eq clatter-message-order 'newest-first))))
         (nick-col
          (let ((column
                 (cond
                  ((eq 'action msg-type)
                   (clatter--format-nick-column
                    "*" 'clatter-action sender))
                  ((eq 'notice msg-type)
                   (clatter--format-nick-column
                    (concat (format "-%s-" sender) bot-tag-delim bot-tag)
                    'clatter-notice))
                  (t
                   (clatter--format-nick-column
                    (concat (clatter--format-sender sender)
                            bot-tag-delim bot-tag)
                    nick-face sender)))))
            (if grouped-p
                (propertize
                 column
                 'clatter-grouped t
                 'display (make-string clatter-nick-column-width ?\s))
              column)))
         (msg-text (prog1 hl-text
                     (cond
                      ((eq 'action msg-type)
                       (add-face-text-property 0 (length hl-text) 'clatter-action nil hl-text))
                      ((eq 'notice msg-type)
                       (add-face-text-property 0 (length hl-text) 'clatter-notice nil hl-text)))
                     (when is-mention
                       (add-face-text-property 0 (length hl-text) 'clatter-mention nil hl-text))))
         ;; Prepend reply context if available
         (reply-line (when reply-context
                       (let* ((ref-sender-text (car reply-context))
                              (ref-sender (car ref-sender-text))
                              (ref-text (cdr ref-sender-text))
                              (ref-msg-type (cdr reply-context))
                              (ref-text-formatted (clatter-format-parse ref-text))
                              (preview (if (> (length ref-text-formatted) 60)
                                           (concat (substring ref-text-formatted 0 57) "...")
                                         ref-text-formatted))
                              (front-nick (if (eq 'action ref-msg-type)
                                              (format "* %s" ref-sender)
                                            (format "%s:" ref-sender)))
                              (front (propertize (format "↳ %s " front-nick) 'face 'font-lock-doc-face)))
                         (add-face-text-property 0 (length preview) 'font-lock-doc-face nil preview)
                         (let ((context (concat front preview))
                               (action
                                (lambda (_button)
                                  (clatter-jump-to-msgid buffer reply-to))))
                           (add-text-properties 0 (length context)
                                                (list 'button '(t)
                                                      'category 'default-button
                                                      'action action
                                                      'follow-link t
                                                      'reply-to reply-to
                                                      'help-echo "Click or press RET to jump to reply context")
                                                context)
                           (concat context "\n")))))
         (formatted
          (cond
           ((eq 'action msg-type)
            (let ((formatted-sender (concat sender " " bot-tag bot-tag-delim)))
              (add-face-text-property 0 (length formatted-sender) 'clatter-action nil formatted-sender)
              (concat (or reply-line "") nick-col " " formatted-sender msg-text)))
           (t
            (concat (or reply-line "") nick-col " " msg-text))))
         (props (list 'clatter-msg-type msg-type
                      'clatter-sender sender
                      'clatter-text text
                      'clatter-server-time server-time)))
    (when msgid
      (setq props (plist-put props 'clatter-msgid msgid)))
    (when self-echo-nonce
      (setq props (plist-put props 'clatter-self-echo-nonce self-echo-nonce)))
    (when extra-props
      (setq props (append props extra-props)))
    (when (and group-gap-p (not insert-before-previous-p))
      (with-current-buffer buffer
        ;; The gap goes on the previous *visible* message's newline: with
        ;; hidden lines (fools, mutes) between it and the insert position,
        ;; the adjacent newline belongs to invisible text and the gap
        ;; would never render.
        (when-let* ((inhibit-read-only t)
                    (bol (clatter--previous-message-bol buffer))
                    (eol (save-excursion (goto-char bol)
                                         (line-end-position))))
          (when (eq (char-after eol) ?\n)
            (put-text-property eol (1+ eol)
                               'line-spacing clatter-group-messages-gap)))))
    (clatter--insert-message
     buffer formatted nil props server-time invisible
     (and group-gap-p insert-before-previous-p clatter-group-messages-gap))
    (when (and (not clatter--suppress-image-scan)
               (not (clatter-fool-p sender (clatter-connection-network-id conn)))
               (fboundp 'clatter-image--scan-message))
      (let ((img-marker (with-current-buffer buffer
                          (copy-marker
                           (or clatter--messages-marker (point-max))))))
        (clatter-image--scan-message text buffer img-marker)))
    (clatter-note-message-time buffer server-time)
    (unless (or (eq buffer (current-buffer))
                (clatter-read-state-message-read-p buffer server-time))
      (clatter-mark-activity buffer is-mention))))

(defun clatter-insert-privmsg (buffer sender text conn &optional server-time invisible)
  "Insert a PRIVMSG from SENDER with TEXT into BUFFER using CONN context.
SERVER-TIME overrides the current time for the timestamp."
  (clatter-insert-generic 'privmsg buffer sender text conn server-time invisible))

(defun clatter-insert-action (buffer sender text conn &optional server-time invisible)
  "Insert a /me ACTION from SENDER with TEXT into BUFFER."
  (clatter-insert-generic 'action buffer sender text conn server-time invisible))

(defun clatter-insert-notice (buffer sender text conn &optional server-time invisible)
  "Insert a NOTICE from SENDER with TEXT into BUFFER."
  (clatter-insert-generic 'notice buffer sender text conn server-time invisible))

(defun clatter-ui--record-pending-self-echo (buffer target sender text msg-type nonce)
  "Record tentative outgoing message metadata for later reconciliation."
  (with-current-buffer buffer
    (when (text-property-any (point-min) (point-max)
                             'clatter-self-echo-nonce nonce)
      (push (list :nonce nonce :target target :sender sender :text text
                  :msg-type msg-type :created-at (float-time))
            clatter--pending-self-echoes))))

(defun clatter-ui--expire-pending-self-echoes (&optional buffer)
  "Discard expired optimistic self echoes from BUFFER.

Their local lines remain visible, but are no longer candidates for server-echo
reconciliation.  BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((cutoff (- (float-time) clatter-self-echo-timeout)))
      (dolist (item clatter--pending-self-echoes)
        (when (<= (plist-get item :created-at) cutoff)
          (when-let* ((start (text-property-any
                              (point-min) (point-max) 'clatter-self-echo-nonce
                              (plist-get item :nonce))))
            (let ((end (next-single-property-change
                        start 'clatter-self-echo-nonce nil (point-max))))
              (with-silent-modifications
                (remove-text-properties start end '(clatter-self-echo-nonce nil)))))))
      (setq clatter--pending-self-echoes
            (cl-remove-if (lambda (item)
                            (<= (plist-get item :created-at) cutoff))
                          clatter--pending-self-echoes)))))

(defun clatter-ui--clear-pending-self-echoes (network-id)
  "Discard optimistic self echoes in all buffers for NETWORK-ID."
  (dolist (buffer (clatter-all-buffers network-id))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (dolist (item clatter--pending-self-echoes)
          (when-let* ((start (text-property-any
                              (point-min) (point-max) 'clatter-self-echo-nonce
                              (plist-get item :nonce))))
            (let ((end (next-single-property-change
                        start 'clatter-self-echo-nonce nil (point-max))))
              (with-silent-modifications
                (remove-text-properties start end '(clatter-self-echo-nonce nil))))))
        (setq clatter--pending-self-echoes nil)))))

(defun clatter-ui--send-privmsg-1 (conn target text &optional msg-type buffer tags)
  "Send TEXT to TARGET and render it according to `clatter-self-echo-mode'.
MSG-TYPE is `privmsg' or `action'; BUFFER receives the local echo.  TAGS, when
non-nil, are the message tags to be affixed to the message."
  (let* ((msg-type (or msg-type 'privmsg))
         (buffer (or buffer (current-buffer)))
         (sender (clatter-connection-nick conn))
         (echo-message-p (member "echo-message" (clatter-connection-cap-enabled conn)))
         (reply-to (or (cdr (assoc "+reply" tags))
                       (cdr (assoc "+draft/reply" tags))
                       (cdr (assoc "draft/reply" tags))))
         (echo-text (copy-sequence text)))
    (when reply-to
      (put-text-property 0 (length echo-text)
                         'clatter-reply-to reply-to echo-text))
    (clatter-send conn (clatter-irc-privmsg
                        target
                        (if (eq 'action msg-type)
                            (clatter-irc-ctcp-action-format text)
                          text)
                        tags))
    (cond
     ;; If the server has echo-message and clatter-self-echo-mode is
     ;; 'optimistic, we can tentatively echo the message locally in the
     ;; expectation we'll receive a server-side echo later.
     ((and echo-message-p (eq clatter-self-echo-mode 'optimistic))
      (let* ((nonce (cl-incf clatter--self-echo-nonce))
             (tentative (propertize echo-text 'clatter-self-echo-nonce nonce)))
        (pcase msg-type
          ('action (clatter-insert-action buffer sender tentative conn))
          (_ (clatter-insert-privmsg buffer sender tentative conn)))
        (clatter-ui--record-pending-self-echo buffer target sender text msg-type nonce)))
     ;; If the server doesn't echo messages, we have to simulate the echo
     ;; by triggering the message hooks.
     ((not echo-message-p)
      (pcase msg-type
        ('action (run-hook-with-args 'clatter-action-hook
                                     conn
                                     (list sender "*" "*") ;; * = Placeholder
                                     target echo-text
                                     nil))
        (_ (run-hook-with-args 'clatter-privmsg-hook
                               conn
                               (list sender "*" "*")       ;; * = Placeholder
                               target echo-text
                               nil)))))))

(defun clatter-ui--send-privmsg (conn target text &optional msg-type buffer tags)
  "Split and send TEXT to TARGET, rendering sent parts according to
`clatter-self-echo-mode'. MSG-TYPE is `privmsg' or `action';  BUFFER
receives the local echo.  TAGS, when non-nil, are the message tags to be
affixed to the message."
  (let* ((command+overhead (concat (clatter-irc-format-tagged-privmsg tags)
                                   ;; Include CTCP ^AACTION ...^A overhead
                                   (when (eq msg-type 'action)
                                     (clatter-irc-ctcp-action-format))))
         (parts (clatter-split-long-message target text command+overhead)))
    (mapc (lambda (part)
            (clatter-ui--send-privmsg-1 conn target part msg-type buffer tags))
          parts)))

(defun clatter-ui--reconcile-self-echo (buffer sender target text msg-type server-time)
  "Reconcile a server echo with its tentative local message in BUFFER.
Matching includes target, sender, message type, and a FIFO pending record, so
identical messages sent close together each reconcile only one local line."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (clatter-ui--expire-pending-self-echoes)
      (let ((pending (cl-find-if
                      (lambda (item)
                        (and (string-equal-ignore-case sender (plist-get item :sender))
                             (string-equal-ignore-case target (plist-get item :target))
                             (eq msg-type (plist-get item :msg-type))
                             (string= text (plist-get item :text))))
                      (reverse clatter--pending-self-echoes))))
        (when pending
          (let ((start (text-property-any (point-min) (point-max)
                                          'clatter-self-echo-nonce
                                          (plist-get pending :nonce))))
            (when start
              (let* ((inhibit-read-only t)
                     (end (next-single-property-change
                           start 'clatter-self-echo-nonce nil (point-max)))
                     (msgid (get-text-property 0 'clatter-msgid text)))
                (remove-text-properties start end '(clatter-self-echo-nonce nil))
                (add-text-properties start end
                                     (list 'clatter-server-time server-time
                                           ;; `clatter-text' is consumed by replies and
                                           ;; message lookup, so update the stored text as
                                           ;; well as the visible tentative line.
                                           'clatter-text text))
                (when msgid
                  (put-text-property start end 'clatter-msgid msgid))
                (when (and server-time (clatter--timestamp-overlay-p))
                  (dolist (overlay (overlays-in start end))
                    (when (overlay-get overlay 'clatter-timestamp)
                      (clatter--timestamp-overlay-apply
                       overlay
                       (format-time-string clatter-timestamp-format server-time)
                       (and clatter-timestamp-tooltip-format
                            (format-time-string
                             clatter-timestamp-tooltip-format server-time))))))
                ;; Do not consume a pending record unless its tentative line
                ;; still exists.  Buffer truncation may have removed it, in
                ;; which case the caller must insert the server echo normally.
                (setq clatter--pending-self-echoes
                      (delq pending clatter--pending-self-echoes))
                t))))))))

(defun clatter-insert-system (buffer text &optional invisible)
  "Insert a system message TEXT into BUFFER."
  (let* ((prefix (clatter--format-system-prefix "***"))
         (formatted (concat prefix " "
                            (prog1 (setq text (copy-sequence text))
                              (add-face-text-property 0 (length text) 'clatter-system t text)))))
    (clatter--insert-message buffer formatted nil nil nil invisible)))

(defun clatter--system-event-add-channel (text channel)
  "Append CHANNEL to compact system event TEXT when available."
  (if (and channel (not (string= channel "")))
      (format "%s %s" text channel)
    text))

(defun clatter--system-event-add-reason (text reason)
  "Append REASON to compact system event TEXT when available."
  (if (and reason (not (string= reason "")))
      (format "%s — %s" text reason)
    text))

(defun clatter--format-system-event (event fields)
  "Format compact EVENT using structured plist FIELDS."
  (let* ((style clatter-compact-system-messages)
         (nick (plist-get fields :nick))
         (channel (plist-get fields :channel))
         (reason (plist-get fields :reason))
         (show-reason (memq style '(reasons full)))
         (show-full (eq style 'full)))
    (pcase event
      ('join
       (let ((text nick)
             (realname (plist-get fields :realname)))
         (when (and show-full realname (not (string= nick realname)))
           (setq text (format "%s (%s)" text realname)))
         (if show-full
             (clatter--system-event-add-channel text channel)
           text)))
      ((or 'part 'quit)
       (let ((text (if show-full
                       (clatter--system-event-add-channel nick channel)
                     nick)))
         (if show-reason
             (clatter--system-event-add-reason text reason)
           text)))
      ('nick
       (format "%s → %s" nick (plist-get fields :new-nick)))
      ((or 'away 'back)
       (let ((text (if show-full
                       (clatter--system-event-add-channel nick channel)
                     nick)))
         (if (and (eq event 'away) show-reason)
             (clatter--system-event-add-reason text reason)
           text)))
      ('mode
       (let ((modes (plist-get fields :modes)))
         (if show-full
             (format "%s %s %s" nick (or channel "") modes)
           (format "%s %s" nick modes))))
      ('kick
       (let* ((setter (plist-get fields :setter))
              (text (format "%s ← %s" nick setter)))
         (when show-full
           (setq text (clatter--system-event-add-channel text channel)))
         (if show-reason
             (clatter--system-event-add-reason text reason)
           text)))
      ('invite
       (if show-full
           (format "%s → %s %s"
                   nick (plist-get fields :invitee) channel)
         (format "%s → %s" nick channel)))
      (_ (or (plist-get fields :verbose) "")))))

(defun clatter--insert-system-event (buffer event fields invisible)
  "Insert structured system EVENT with FIELDS into BUFFER.
INVISIBLE carries the same message categories as `clatter-insert-system'."
  (unless (and (memq event '(join part quit))
               (clatter-fool-p
                (plist-get fields :nick)
                (buffer-local-value 'clatter--network buffer)))
    (if (null clatter-compact-system-messages)
        (clatter-insert-system buffer (plist-get fields :verbose) invisible)
      (let* ((symbol (or (alist-get event clatter-compact-system-symbols) "***"))
             (prefix (clatter--format-system-prefix symbol))
             (text (clatter--format-system-event event fields))
             (formatted (concat prefix " "
                                (prog1 (setq text (copy-sequence text))
                                  (add-face-text-property
                                   0 (length text) 'clatter-system t text)))))
        (if (and (eq clatter-compact-system-messages 'compact)
                 (memq event '(join part quit away back))
                 (clatter--append-compact-system-group
                  buffer event text invisible))
            nil
          (let ((group-id (and (eq clatter-compact-system-messages 'compact)
                               (memq event '(join part quit away back))
                               (cl-incf clatter--compact-system-group-id))))
            (clatter--insert-message
             buffer formatted nil
             (and group-id (list 'clatter-compact-system-group-id group-id))
             nil invisible)
            (when group-id
              (clatter--record-compact-system-group
               buffer group-id invisible))))))))

(defun clatter--compact-system-visibility (invisible)
  "Return grouping visibility from event INVISIBLE categories."
  (seq-remove (lambda (category)
                (memq category '(join part quit away back)))
              (ensure-list invisible)))

(defun clatter--refresh-compact-system-layout ()
  "Refresh separators and timestamps in compact system groups.

Compact actions remain independently suppressible.  This function keeps the
shared layout coherent when `buffer-invisibility-spec' changes, notably when
`visible-mode' is disabled and restores Clatter's suppression categories."
  (when (derived-mode-p 'clatter-mode)
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (position (point-min)))
      (while-let ((group-start
                   (text-property-not-all
                    position (point-max)
                    'clatter-compact-system-group-id nil)))
        (let* ((group-end (or (next-single-property-change
                               group-start 'clatter-compact-system-group-id
                               nil (point-max))
                              (point-max)))
               (newline-position
                (and (> group-end group-start)
                     (eq (char-before group-end) ?\n)
                     (1- group-end)))
               (event-position group-start)
               (first-event-start nil)
               (previous-event-end nil)
               (any-visible nil))
          ;; Normalize the whole logical line, including segments inserted by
          ;; an earlier loaded implementation.  Property gaps cause Visual
          ;; Line mode to wrap those segments at column zero, inside the nick
          ;; column.
          (add-text-properties
           group-start group-end
           (list 'wrap-prefix
                 (make-string (1+ clatter-nick-column-width) ?\s)
                 'line-prefix ""))
          (while-let ((event-start
                       (text-property-not-all
                        event-position group-end
                        'clatter-compact-system-event nil)))
            (let* ((event-end (or (next-single-property-change
                                   event-start 'clatter-compact-system-event
                                   nil group-end)
                                  group-end))
                   (event-visible (not (invisible-p event-start))))
              (unless first-event-start
                (setq first-event-start event-start))
              (when previous-event-end
                (when-let* ((separator-start
                             (text-property-not-all
                              previous-event-end event-start
                              'clatter-compact-system-separator nil)))
                  (let ((separator-end
                         (or (next-single-property-change
                              separator-start
                              'clatter-compact-system-separator nil event-start)
                             event-start)))
                    (put-text-property separator-start separator-end 'display
                                       (unless (and any-visible event-visible)
                                         ""))
                    (when (and any-visible event-visible)
                      (remove-text-properties
                       separator-start separator-end '(invisible nil))))))
              (when event-visible
                (setq any-visible t))
              (setq previous-event-end event-end
                    event-position event-end)))
          ;; The indentation belongs to the group rather than its first
          ;; action.  If that action is hidden but a later one remains visible,
          ;; expose the indentation so the later action stays out of the nick
          ;; column.  Restore its original categories when the whole group is
          ;; hidden again.
          (when first-event-start
            (put-text-property
             group-start first-event-start 'invisible
             (unless any-visible
               (get-text-property
                group-start 'clatter-compact-system-prefix-invisible)))
            (when any-visible
              (remove-text-properties group-start first-event-start
                                      '(display nil))))
          (dolist (overlay (append (overlays-at group-start)
                                   (and newline-position
                                        (overlays-in (1- newline-position)
                                                     (1+ newline-position)))))
            (when (overlay-get overlay 'clatter-timestamp)
              (overlay-put
               overlay 'invisible
               (unless any-visible
                 (overlay-get overlay
                              'clatter-compact-system-invisible)))))
          ;; Keep a visible group separated from the following line, but
          ;; collapse its boundary again when every action becomes hidden.
          (when newline-position
            (if any-visible
                (remove-text-properties newline-position group-end
                                        '(invisible nil display nil))
              (put-text-property
               newline-position group-end 'invisible
               (and first-event-start
                    (get-text-property first-event-start 'invisible)))))
          (when newline-position
            (clatter--timestamp-inline-refresh-at newline-position))
          (setq position group-end)))
      (clatter--refresh-input-spacers (current-buffer)))))

(defun clatter--end-compact-system-group ()
  "Forget the current buffer's pending compact system group."
  (when-let* ((tail (plist-get clatter--compact-system-group :tail)))
    (when (markerp tail)
      (set-marker tail nil)))
  (setq clatter--compact-system-group nil))

(defun clatter--compact-system-group-tail-valid-p (group buffer)
  "Return non-nil when GROUP still ends its original line in BUFFER."
  (let ((tail (plist-get group :tail))
        (group-id (plist-get group :id)))
    (and group-id
         (markerp tail)
         (eq (marker-buffer tail) buffer)
         (with-current-buffer buffer
           (let ((position (marker-position tail)))
             (and (< position (point-max))
                  (eq (char-after position) ?\n)
                  (= position
                     (save-excursion
                       (goto-char position)
                       (line-end-position)))
                  (save-excursion
                    (goto-char position)
                    (text-property-any
                     (line-beginning-position) (1+ position)
                     'clatter-compact-system-group-id group-id))
                  ;; Do not append to a compact group created by an older
                  ;; loaded implementation that lacks per-event metadata.
                  (save-excursion
                    (goto-char position)
                    (text-property-not-all
                     (line-beginning-position) position
                     'clatter-compact-system-event nil))
                  ;; A stale marker must never be allowed to enter the
                  ;; editable prompt, regardless of message ordering.
                  (or (not clatter--prompt-marker)
                      (if (eq clatter-message-order 'oldest-first)
                          (< position (marker-position clatter--prompt-marker))
                        (and clatter--messages-marker
                             (>= (save-excursion
                                   (goto-char position)
                                   (line-beginning-position))
                                 (marker-position
                                  clatter--messages-marker)))))))))))

(defun clatter--append-compact-system-group (buffer event text invisible)
  "Append TEXT to BUFFER's compatible compact EVENT group.
Return non-nil when the event was grouped.  Each action retains its own
INVISIBLE categories so smart-hidden and visible actions can share a group."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((group clatter--compact-system-group)
             (tail (plist-get group :tail)))
        (when (and group
                   (= clatter--message-generation
                      (plist-get group :generation))
                   (clatter--compact-system-group-tail-valid-p group buffer))
          (let ((now (clatter--compact-system-now)))
            (when (<= (- now (plist-get group :time))
                      clatter-compact-system-group-window)
              (let ((pre-input (and clatter--input-marker
                                    (marker-position clatter--input-marker)))
                    (start (marker-position tail))
                    (symbol (or (alist-get event clatter-compact-system-symbols)
                                "***"))
                    (separator-invisible
                     (delete-dups
                      (append (ensure-list (plist-get group :last-invisible))
                              (ensure-list invisible)
                              nil))))
                (let ((inhibit-read-only t)
                      (buffer-undo-list t))
                  (save-excursion
                    (goto-char tail)
                    (insert
                     (propertize clatter-compact-system-separator
                                 'invisible separator-invisible
                                 'clatter-compact-system-separator t)
                     (propertize symbol
                                 'invisible invisible
                                 'clatter-compact-system-event t)
                     (propertize " "
                                 'invisible invisible
                                 'clatter-compact-system-event t)
                     (propertize text
                                 'invisible invisible
                                 'clatter-compact-system-event t))
                    (add-text-properties
                     start (point)
                     (list 'face 'clatter-system
                           'clatter-compact-system-group-id
                           (plist-get group :id)
                           'read-only t
                           'front-sticky nil
                           'rear-nonsticky t
                           'wrap-prefix
                           (make-string (1+ clatter-nick-column-width) ?\s)
                           'line-prefix ""))
                    (set-marker tail (point))
                    (clatter--timestamp-inline-refresh-at (point))))
                (when (and pre-input clatter--input-marker)
                  (clatter--update-undo-list
                   (- (marker-position clatter--input-marker) pre-input)))
                (setf (plist-get clatter--compact-system-group :time) now)
                (setf (plist-get clatter--compact-system-group :last-invisible)
                      invisible)
                (clatter--refresh-compact-system-layout)
                t))))))))

(defun clatter--record-compact-system-group (buffer group-id invisible)
  "Record BUFFER's newly inserted compact GROUP-ID."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((start (text-property-any
                    (point-min) (point-max)
                    'clatter-compact-system-group-id group-id)))
        (when start
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char start)
              (let ((end (line-end-position)))
                (put-text-property start end 'invisible invisible)
                (when-let* ((event-start
                             (text-property-not-all
                              start end 'clatter-navigation-target nil)))
                  (put-text-property
                   start event-start 'invisible
                   (clatter--compact-system-visibility invisible))
                  (put-text-property
                   start event-start 'clatter-compact-system-prefix-invisible
                   (clatter--compact-system-visibility invisible))
                  (put-text-property event-start end
                                     'clatter-compact-system-event t))
                (put-text-property end (min (1+ end) (point-max))
                                   'invisible
                                   (clatter--compact-system-visibility invisible))
                (dolist (overlay (append (overlays-at start)
                                         (overlays-in (max (point-min) (1- end))
                                                      (min (point-max) (1+ end)))))
                  (when (overlay-get overlay 'clatter-timestamp)
                    (overlay-put overlay 'invisible invisible)
                    (overlay-put overlay 'clatter-compact-system-invisible
                                 invisible))))
              (setq clatter--compact-system-group
                    (list :id group-id
                          :last-invisible invisible
                          :generation clatter--message-generation
                          :time (clatter--compact-system-now)
                          ;; Move this marker explicitly after each append;
                          ;; insertion-type nil prevents unrelated edits at
                          ;; the boundary from silently relocating it.
                          :tail (copy-marker (line-end-position))))
              (clatter--refresh-compact-system-layout))))))))

(defun clatter-insert-error (buffer text)
  "Insert an error message TEXT into BUFFER."
  (let* ((prefix (clatter--format-nick-column "!!!" 'clatter-error))
         (formatted (concat prefix " "
                            (propertize text 'face 'clatter-error))))
    (clatter--insert-message buffer formatted)))

;; --- Input prompt ---
(defvar clatter-input-formatting-mode-map
  (let ((map (make-sparse-keymap))
        (prefix (make-sparse-keymap)))
    (define-key prefix (kbd "b") #'clatter-input-bold)
    (define-key prefix (kbd "i") #'clatter-input-italic)
    (define-key prefix (kbd "u") #'clatter-input-underline)
    (define-key prefix (kbd "s") #'clatter-input-strikethrough)
    (define-key prefix (kbd "m") #'clatter-input-monospace)
    (define-key prefix (kbd "v") #'clatter-input-reverse)
    (define-key prefix (kbd "r") #'clatter-input-reset)
    (define-key prefix (kbd "c") #'clatter-input-color)
    (define-key map (kbd "C-c C-f") prefix)
    map)
  "Keymap for `clatter-input-formatting-mode'.")

(defvar-local clatter-input-formatting--overlays nil
  "Overlays rendering IRC formatting in the current input.")

(defun clatter-input-formatting--clear ()
  "Delete overlays created by `clatter-input-formatting-mode'."
  (mapc #'delete-overlay clatter-input-formatting--overlays)
  (setq clatter-input-formatting--overlays nil))

(defun clatter-input-formatting--input-bounds ()
  "Return the live Clatter input bounds, or nil."
  (when (and (markerp clatter--input-marker)
             (marker-position clatter--input-marker)
             (markerp clatter--messages-marker)
             (marker-position clatter--messages-marker))
    (cons (marker-position clatter--input-marker) (clatter--input-end))))

(defun clatter-input-formatting--refresh ()
  "Render IRC formatting in the current input."
  (clatter-input-formatting--clear)
  (let ((bounds (clatter-input-formatting--input-bounds)))
    (when bounds
      (setq clatter-input-formatting--overlays
            (clatter-format-propertize-region (car bounds) (cdr bounds))))))

(defun clatter-input-formatting--after-change (beg end _old-len)
  "Refresh input formatting when a change from BEG to END touches the input."
  (let ((bounds (clatter-input-formatting--input-bounds)))
    (when (and bounds
               (<= beg (cdr bounds))
               (>= end (car bounds)))
      ;; ponytail: IRC state is prefix-dependent; rescan short drafts, add checkpoints only if measured.
      (clatter-input-formatting--refresh))))

(define-minor-mode clatter-input-formatting-mode
  "Render literal IRC formatting controls in the editable input."
  :init-value nil
  :lighter nil
  :keymap clatter-input-formatting-mode-map
  (if clatter-input-formatting-mode
      (progn
        (add-hook 'after-change-functions
                  #'clatter-input-formatting--after-change nil t)
        (add-hook 'change-major-mode-hook
                  #'clatter-input-formatting--clear nil t)
        (clatter-input-formatting--refresh))
    (remove-hook 'after-change-functions
                 #'clatter-input-formatting--after-change t)
    (remove-hook 'change-major-mode-hook
                 #'clatter-input-formatting--clear t)
    (clatter-input-formatting--clear)))

(defun clatter-input-formatting--insert (opening &optional closing)
  "Insert OPENING at the effective input point.
When CLOSING and an active input region exist, wrap it with both strings."
  (let ((bounds (clatter-input-formatting--input-bounds)))
    (unless bounds
      (user-error "Current buffer has no Clatter input area"))
    (if (and closing (use-region-p))
        (let ((region-start (region-beginning))
              (region-end (region-end)))
          (unless (and (<= (car bounds) region-start)
                       (<= region-end (cdr bounds)))
            (user-error "Region must be inside the Clatter input area"))
          (let ((start-marker (copy-marker region-start))
                (end-marker (copy-marker region-end t)))
            (unwind-protect
                (progn
                  (atomic-change-group
                    (goto-char end-marker)
                    (insert closing)
                    (goto-char start-marker)
                    (insert opening))
                  (goto-char end-marker)
                  (deactivate-mark))
              (set-marker start-marker nil)
              (set-marker end-marker nil))))
      (unless (and (<= (car bounds) (point))
                   (<= (point) (cdr bounds)))
        (goto-char (cdr bounds)))
      (insert opening))))

(defun clatter-input-bold ()
  "Insert or wrap the input with an IRC bold toggle."
  (interactive)
  (clatter-input-formatting--insert
   (string clatter-format--bold) (string clatter-format--bold)))

(defun clatter-input-italic ()
  "Insert or wrap the input with an IRC italic toggle."
  (interactive)
  (clatter-input-formatting--insert
   (string clatter-format--italic) (string clatter-format--italic)))

(defun clatter-input-underline ()
  "Insert or wrap the input with an IRC underline toggle."
  (interactive)
  (clatter-input-formatting--insert
   (string clatter-format--underline) (string clatter-format--underline)))

(defun clatter-input-strikethrough ()
  "Insert or wrap the input with an IRC strikethrough toggle."
  (interactive)
  (clatter-input-formatting--insert
   (string clatter-format--strikethrough)
   (string clatter-format--strikethrough)))

(defun clatter-input-monospace ()
  "Insert or wrap the input with an IRC monospace toggle."
  (interactive)
  (clatter-input-formatting--insert
   (string clatter-format--monospace) (string clatter-format--monospace)))

(defun clatter-input-reverse ()
  "Insert or wrap the input with an IRC reverse-video toggle."
  (interactive)
  (clatter-input-formatting--insert
   (string clatter-format--reverse) (string clatter-format--reverse)))

(defun clatter-input-reset ()
  "Insert an IRC formatting reset in the input."
  (interactive)
  (clatter-input-formatting--insert (string clatter-format--reset)))

(defun clatter-input-color ()
  "Insert or wrap the input with an indexed IRC color."
  (interactive)
  (unless (clatter-input-formatting--input-bounds)
    (user-error "Current buffer has no Clatter input area"))
  (let* ((palette
          (cl-loop for index below 99
                   collect
                   (format "%02d %s %s"
                           index
                           (clatter-format--color-name-for-index index)
                           (clatter-format--color-for-index index))))
         (completion-extra-properties
          (list
           :annotation-function
           (lambda (candidate)
             (unless (member candidate '("default" "none"))
               (concat
                " "
                (propertize
                 "  " 'face
                 (list :background
                       (clatter-format--color-for-index
                        (string-to-number candidate)))))))))
         (foreground
          (completing-read "Foreground: " (cons "default" palette) nil t))
         (background
          (unless (equal foreground "default")
            (completing-read "Background: " (cons "none" palette) nil t)))
         (opening
          (concat (string clatter-format--color)
                  (unless (equal foreground "default")
                    (concat (substring foreground 0 2)
                            (unless (equal background "none")
                              (concat "," (substring background 0 2))))))))
    (clatter-input-formatting--insert
     opening (string clatter-format--color))))

(defun clatter--align-prompt (prompt)
  "Right-align the visible part of PROMPT in the nick column.
Trailing spaces or tabs are kept after the column.  This lets formats such as
`%t> ' use their final space as the same separator used between a sender nick
and message text."
  (let* ((trailing-start
          (save-match-data
            (if (string-match "[ \t]*\\'" prompt)
                (match-beginning 0)
              (length prompt))))
         (visible (substring prompt 0 trailing-start))
         (trailing (substring prompt trailing-start))
         (padding (max 0 (- clatter-nick-column-width
                            (string-width visible)))))
    (concat (make-string padding ?\s) visible trailing)))

(defun clatter--prompt-string (&optional buffer)
  "Return the configured prompt string for BUFFER.
When BUFFER is nil, use the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((target (or clatter--target "clatter"))
           (network (or clatter--network ""))
           (conn (and clatter--network
                      (clatter-get-connection clatter--network)))
           (nick (if conn (or (clatter-connection-nick conn) "") ""))
           (away-p (and conn (clatter-connection-away-p conn)))
           (away (if away-p clatter-away-indicator ""))
           (format-spec `((?t . ,target) (?n . ,nick) (?N . ,network)
                          (?a . ,away) (?% . "%"))))
      (cond
       ((stringp clatter-prompt-format)
        (format-spec clatter-prompt-format format-spec))
       ((functionp clatter-prompt-format)
        (let ((result (funcall clatter-prompt-format (current-buffer))))
          (unless (stringp result)
            (error "`clatter-prompt-format' function must return a string"))
          result))
       (t (error "Invalid `clatter-prompt-format': %S" clatter-prompt-format))))))

(defun clatter--prompt-format-needs-nick-p ()
  "Return non-nil if `clatter-prompt-format' may depend on the current nick."
  (or (functionp clatter-prompt-format)
      (and (stringp clatter-prompt-format)
           (string-match-p "\\(?:^\\|[^%]\\)\\(?:%%\\)*%n"
                           clatter-prompt-format))))

(defun clatter--prompt-format-needs-away-p ()
  "Return non-nil if `clatter-prompt-format' may depend on the current away
state."
  (or (functionp clatter-prompt-format)
      (and (stringp clatter-prompt-format)
           (string-match-p "\\(?:^\\|[^%]\\)\\(?:%%\\)*%a"
                           clatter-prompt-format))))

(defun clatter--prompt-shows-nick-p (prompt &optional buffer)
  "Return non-nil if PROMPT displays BUFFER's current connection nick."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((conn (and clatter--network
                      (clatter-get-connection clatter--network)))
           (nick (and conn (clatter-connection-nick conn))))
      (and (stringp nick)
           (not (string-empty-p nick))
           (if (stringp clatter-prompt-format)
               (string-match-p "\\(?:^\\|[^%]\\)\\(?:%%\\)*%n"
                               clatter-prompt-format)
             (string-match-p (regexp-quote nick) prompt))))))

(defun clatter--propertized-prompt (&optional buffer)
  "Return the read-only, propertized prompt for BUFFER."
  (let* ((prompt (clatter--prompt-string buffer))
         (prompt (if (eq clatter-prompt-alignment 'right)
                     (clatter--align-prompt prompt)
                   prompt)))
    (with-current-buffer (or buffer (current-buffer))
      (setq-local clatter--prompt-shows-nick
                  (clatter--prompt-shows-nick-p prompt)))
    (propertize prompt
                'face 'clatter-prompt
                'read-only t
                ;; A field boundary makes the line-motion primitives stop at
                ;; the input origin, so third-party editing commands built on
                ;; them (evil's `dd', `kill-whole-line', ...) cannot reach
                ;; into the prompt and fail on its read-only text.
                'field 'clatter-prompt
                'inhibit-line-move-field-capture t
                'front-sticky t
                'rear-nonsticky t)))

(defun clatter--refresh-prompt ()
  "Refresh the current buffer's prompt without losing pending input."
  (when (and clatter--prompt-marker clatter--input-marker)
    (let* ((input (clatter--get-input))
           (point-in-input (and (>= (point) (marker-position clatter--input-marker))
                                (<= (point) (clatter--input-end))))
           (input-offset (and point-in-input
                              (- (point) (marker-position clatter--input-marker))))
           (inhibit-read-only t))
      (save-excursion
        (goto-char clatter--prompt-marker)
        ;; Bottom-prompt markers advance on insertion (type t), so inserting
        ;; the new prompt would carry them past it: the next refresh would
        ;; then delete an empty region and append a duplicate prompt, while
        ;; new messages land below the input line.  Pin them back afterwards.
        (let ((prompt-pos (marker-position clatter--prompt-marker))
              (messages-pos (and (eq clatter-message-order 'oldest-first)
                                 clatter--messages-marker
                                 (marker-position clatter--messages-marker))))
          (delete-region clatter--prompt-marker clatter--input-marker)
          (insert (clatter--propertized-prompt))
          (set-marker clatter--prompt-marker prompt-pos)
          (when messages-pos
            (set-marker clatter--messages-marker messages-pos))
          (set-marker clatter--input-marker (point))))
      ;; INPUT remains after the newly inserted prompt.  Restore point in it
      ;; so a nick change cannot disrupt someone composing a message.
      (when point-in-input
        (goto-char (+ (marker-position clatter--input-marker)
                      (min input-offset (length input)))))
      (clatter--refresh-input-spacers (current-buffer)))))

(defun clatter--insert-typing-indicator-line ()
  "Insert and overlay a protected blank typing-indicator row at point."
  (let ((start (point)))
    (insert (propertize "\n"
                        'read-only t
                        'front-sticky t
                        'rear-nonsticky t
                        'field 'clatter-messages
                        'inhibit-line-move-field-capture t
                        'clatter-typing-indicator-line t))
    ;; Messages inserted at START must stay before this row in oldest-first
    ;; buffers, hence the advancing overlay front.
    (setq clatter--typing-indicator-overlay
          (make-overlay start (point) nil t nil))
    (overlay-put clatter--typing-indicator-overlay
                 'clatter-typing-indicator t)))

(defun clatter--setup-prompt (buffer)
  "Set up the input prompt in BUFFER.
For `newest-first' the prompt sits at the top with messages below it.
For `oldest-first' the prompt is anchored at the bottom, like a
conventional IRC client, with messages accumulating above it."
  (with-current-buffer buffer
    (let* ((inhibit-read-only t)
           (buffer-undo-list t)
           (prompt (clatter--propertized-prompt buffer))
           (typing-row-p
            (and (eq clatter-typing-indicator-location 'input-separator)
                 (memq clatter--buffer-type '(channel query)))))
      (setq-local wrap-prefix (make-string (length prompt) ?\s))
      (clatter-input-ring-setup)
      (if (eq clatter-message-order 'oldest-first)
          ;; Bottom prompt: [messages...] then prompt+input on the last line.
          (progn
            (goto-char (point-min))
            (setq clatter--input-padding-end (point-marker))
            (set-marker-insertion-type clatter--input-padding-end nil)
            ;; Messages insert before the optional typing row and prompt.
            (setq clatter--messages-marker (point-marker))
            (when typing-row-p
              (clatter--insert-typing-indicator-line))
            (setq clatter--prompt-marker (point-marker))
            (insert prompt)
            (setq clatter--input-marker (point-marker))
            (set-marker-insertion-type clatter--input-marker nil)
            ;; Type t so each inserted message advances the markers, keeping
            ;; them (and the prompt) just below the growing message area.
            (set-marker-insertion-type clatter--messages-marker t)
            (set-marker-insertion-type clatter--prompt-marker t))
        ;; Top prompt: prompt+input on line 1, messages below.
        (goto-char (point-min))
        (setq clatter--input-padding-end nil)
        (setq clatter--prompt-marker (point-marker))
        (set-marker-insertion-type clatter--prompt-marker nil)
        (insert prompt)
        (setq clatter--input-marker (point-marker))
        (set-marker-insertion-type clatter--input-marker nil)
        ;; Newline separates input from the optional typing row and messages.
        (save-excursion
          (goto-char clatter--input-marker)
          (insert (propertize "\n"
                              'read-only t
                              ;; Close the input field so `field-end' and
                              ;; friends stop before the separator newline.
                              'field 'clatter-messages
                              'inhibit-line-move-field-capture t
                              'rear-nonsticky t))
          (when typing-row-p
            (clatter--insert-typing-indicator-line))
          (setq clatter--messages-marker (point-marker))
          (set-marker-insertion-type clatter--messages-marker nil)))
      (goto-char clatter--input-marker)
      (add-hook 'pre-command-hook #'clatter--move-to-prompt nil t)
      (clatter--refresh-input-spacers buffer)
      (when clatter-input-formatting-mode
        (clatter-input-formatting--refresh)))))

(defun clatter--input-end ()
  "Return the buffer position just past the user input.
For a bottom (oldest-first) prompt this is `point-max'; for a top
prompt it is just before the newline that separates input from
messages."
  (if (eq clatter-message-order 'oldest-first)
      (point-max)
    (if (overlayp clatter--typing-indicator-overlay)
        (1- (overlay-start clatter--typing-indicator-overlay))
      (1- (marker-position clatter--messages-marker)))))

(defun clatter--get-input ()
  "Get user input text from the prompt."
  (when (and clatter--input-marker clatter--messages-marker)
    (buffer-substring-no-properties
     clatter--input-marker
     (clatter--input-end))))

(defun clatter--clear-input ()
  "Clear the user input area."
  (when (and clatter--input-marker clatter--messages-marker)
    (let ((inhibit-read-only t))
      (delete-region clatter--input-marker (clatter--input-end)))
    (clatter--refresh-input-spacers (current-buffer))))

(defun clatter--set-input (input)
  "Replace the prompt input with INPUT."
  (clatter--clear-input)
  (when clatter--input-marker
    (goto-char clatter--input-marker)
    (insert input)
    (clatter--refresh-input-spacers (current-buffer))))

(defun clatter--move-to-prompt ()
  "Move point to the input line before a self-inserting command.
Installed on `pre-command-hook' so typing anywhere in the buffer starts
editing at the prompt, like `erc-move-to-prompt'.  Controlled by
`clatter-move-to-prompt'."
  (when (and clatter-move-to-prompt
             clatter--input-marker
             (eq this-command 'self-insert-command)
             (or (< (point) (marker-position clatter--input-marker))
                 (> (point) (clatter--input-end))))
    (goto-char (clatter--input-end))))

(defun clatter-set-prev-input ()
  "Insert the previous (older) input history item at the prompt."
  (interactive)
  (when (and (ring-p clatter-input-ring)
             (not (ring-empty-p clatter-input-ring)))
    (setq clatter-input-ring-index
          (min (1+ clatter-input-ring-index)
               (1- (ring-length clatter-input-ring))))
    (let ((item (clatter-input-ring-nth clatter-input-ring-index)))
      (when item
        (clatter--set-input item)))))

(defun clatter-set-next-input ()
  "Insert the next (newer) input history item at the prompt."
  (interactive)
  (when (and (ring-p clatter-input-ring)
             (not (ring-empty-p clatter-input-ring)))
    (setq clatter-input-ring-index (max 0 (1- clatter-input-ring-index)))
    (let ((item (clatter-input-ring-nth clatter-input-ring-index)))
      (when item
        (clatter--set-input item)))))

(defun clatter-echo-history-position (direction func &rest args)
  "Calls FUNC with ARGS assuming it shifts the input ring position in DIRECTION.
Echoes a message describing the current input ring position."
  (when (and clatter-input-ring (ring-p clatter-input-ring))
    (let* ((before clatter-input-ring-index)
           (after (progn (apply func args) clatter-input-ring-index))
           (total (ring-length clatter-input-ring))
           (current (- total clatter-input-ring-index)))
      (cond
       ((zerop total)
        (message "History item: -/- [Empty]"))
       ((= before after)
        (message "History item: %d/%d [%s]" current total direction))
       (t (message "History item: %d/%d" current total))))))

(defun clatter-echo-history-position-prev (func &rest args)
  "Calls FUNC with ARGS, it modifies the input ring position in upwards.
Echoes a message describing the current input ring position."
  (apply #'clatter-echo-history-position "Top" func args))

(defun clatter-echo-history-position-next (func &rest args)
  "Calls FUNC with ARGS, it modifies the input ring position in downwards.
Echoes a message describing the current input ring position."
  (apply #'clatter-echo-history-position "Bottom" func args))

;; Install middleware-like advice functions that echo the history positions.
(advice-add 'clatter-set-prev-input :around #'clatter-echo-history-position-prev)
(advice-add 'clatter-set-next-input :around #'clatter-echo-history-position-next)

;; --- Input handling ---

(defun clatter-send-input ()
  "Send the current input line.
If the input contains multiple lines and exceeds
`clatter-paste-flood-threshold', prompt before sending."
  (interactive)
  (let ((input (string-trim (or (clatter--get-input) ""))))
    (when (> (length input) 0)
      (clatter-input-ring-add input)
      (let* ((lines (split-string input "\n"))
             (nlines (length lines))
             (flood (and clatter-paste-flood-threshold
                         (> nlines clatter-paste-flood-threshold))))
        (if (and flood
                 (not (y-or-n-p
                       (format "Paste %d lines to %s? "
                               nlines (or clatter--target "?")))))
            (message "[clatter] Paste cancelled")
          (clatter--clear-input)
          (setq buffer-undo-list nil)
          (if (string-prefix-p "/" (car lines))
              (clatter--handle-command (car lines))
            (dolist (line lines)
              (let ((trimmed (string-trim line)))
                (when (> (length trimmed) 0)
                  (clatter--send-message trimmed)))))
          (clatter--send-typing-done))))))

(defun clatter--send-message (text)
  "Send TEXT as a PRIVMSG to the current target."
  (let* ((network clatter--network)
         (target clatter--target)
         (conn (clatter-get-connection network)))
    (when (and conn target (not (string= target "*server*")))
      (clatter-ui--send-privmsg conn target text 'privmsg (current-buffer)))))

(defun clatter--handle-command (input)
  "Parse and execute INPUT as a /command."
  ;; Forward to clatter-commands.el
  (clatter-execute-command input))

(defun clatter-bol ()
  "Move `point' to the beginning of the current line.
With `visual-line-mode' enabled, move to the beginning of the current
visual line instead.  Either way the prompt's field property keeps
point out of the prompt, so on the prompt line this stops at the start
of the input."
  (interactive)
  (if visual-line-mode
      (beginning-of-visual-line)
    (move-beginning-of-line 1))
  ;; Field motion stops at the input origin when coming from the input,
  ;; but a click can leave point inside the prompt itself; snap out.
  (when (equal (point-marker) clatter--prompt-marker)
    (goto-char clatter--input-marker)))

(defun clatter--clamp-to-input (position)
  "Clamp POSITION to the bounds of the input area."
  (max (marker-position clatter--input-marker)
       (min position (clatter--input-end))))

(defun clatter-backward-kill-word (&optional arg)
  "Kill ARG words backward without crossing the start of the input area.
Point stops at the input origin instead of drifting into the message
area, matching what the read-only prompt does for a plain
\\[delete-backward-char].  A negative ARG kills forward, stopping at the
end of the input.  Outside the input area this is `backward-kill-word'."
  (interactive "p")
  (let ((arg (or arg 1)))
    (if (clatter-in-input-p)
        (let* ((from (point))
               (to (clatter--clamp-to-input
                    (save-excursion
                      (ignore-errors (backward-word arg))
                      (point)))))
          (unless (= from to)
            (kill-region to from)))
      (backward-kill-word arg))))

(defun clatter-kill-word (&optional arg)
  "Kill ARG words forward without crossing the end of the input area.
Outside the input area this is `kill-word'."
  (interactive "p")
  (if (clatter-in-input-p)
      (clatter-backward-kill-word (- (or arg 1)))
    (kill-word (or arg 1))))

;; Forward declaration
(declare-function clatter-execute-command "clatter-commands")

;; --- Mode-line ---

(defvar clatter-mode-line-format
  '(:eval (clatter--mode-line-string))
  "Mode-line construct for clatter buffers.")

(defun clatter--header-line-inject-tooltip (line)
  "Extend LINE with a self-descriptive tooltip."
  (when line
    (put-text-property
     0 (length line)
     ;; Ensure tooltip text is filled to a fixed column in order
     ;; to avoid generating long single-line tooltips.
     'help-echo (with-temp-buffer
                  (insert line)
                  (fill-region (point-min) (point-max))
                  (buffer-string))
     line))
  line)

(defun clatter--header-line-string ()
  "Generate header-line channel context for the current Clatter buffer."
  (when clatter--network
    (let* ((target (or clatter--target ""))
           (base (format "[%s/%s]" clatter--network target))
           (modes (and clatter--channel-modes
                       (not (string-empty-p clatter--channel-modes))
                       clatter--channel-modes))
           (nicks (clatter-nick-count (current-buffer)))
           (details (string-join
                     (delq nil
                           (list modes
                                 (when (> nicks 0)
                                   (format "%d %s" nicks
                                           (if (= nicks 1) "nick" "nicks")))))
                     " "))
           (context (if (string-empty-p details)
                        base
                      (format "%s %s" base details))))
      (clatter--header-line-inject-tooltip
       (if (and clatter--topic (not (string-empty-p clatter--topic)))
           (format "%s - %s" context clatter--topic)
         context)))))

(defun clatter--header-line-topic-string ()
  "Generate the topic-only header-line preset for the current buffer.
Fall back to the network and target when the buffer has no topic."
  (when clatter--network
    (clatter--header-line-inject-tooltip
     (if (and clatter--topic (not (string-empty-p clatter--topic)))
         clatter--topic
       (format "[%s/%s]" clatter--network (or clatter--target ""))))))

(defun clatter--effective-header-line-preset ()
  "Return the configured header-line preset."
  clatter-header-line-preset)

(defun clatter--effective-header-line-format ()
  "Return the header-line construct selected by the preset."
  (pcase (clatter--effective-header-line-preset)
    ('topic '(:eval (clatter--header-line-topic-string)))
    ('context '(:eval (clatter--header-line-string)))
    (_ nil)))

(defun clatter--mode-line-state-fragment (conn)
  "Return a propertized connection-state fragment for CONN, or nil.
Shown in the mode-line so a dead or reconnecting network is visible at a
glance without running `clatter-status'.  Returns nil when CONN is fully
connected (the common case) or absent."
  (when conn
    (let ((state (clatter-connection-state conn)))
      (pcase state
        (:connected nil)
        (:connecting (propertize "(connecting)" 'face 'clatter-error))
        (:registering (propertize "(connecting)" 'face 'clatter-error))
        (:disconnected
         (let ((timer (clatter-connection-reconnect-timer conn)))
           (cond
            ((and timer
                  (clatter-connection-reconnect-enabled conn))
             (let ((remaining (max 0 (round (- (float-time (timer--time timer))
                                               (float-time))))))
               (propertize (format "(reconnect %ds)" remaining)
                           'face 'clatter-error)))
            ((clatter-connection-reconnect-enabled conn)
             (propertize "(reconnecting)" 'face 'clatter-error))
            (t (propertize "(off)" 'face 'clatter-error)))))))))

(defun clatter--mode-line-string ()
  "Generate mode-line string for current clatter buffer."
  (when clatter--network
    (let* ((preset (clatter--effective-header-line-preset))
           (show-identity (not (eq preset 'context)))
           (show-nicks (not (eq preset 'context)))
           (conn (clatter-get-connection clatter--network))
           (nick (if conn (clatter-connection-nick conn) "?"))
           (nick-str (unless clatter--prompt-shows-nick nick))
           (away-str (unless (clatter--prompt-format-needs-away-p)
                       (and conn
                            (clatter-connection-away-p conn)
                            clatter-away-indicator)))
           (nicks (clatter-nick-count (current-buffer)))
           (topic-str (if (and (not (memq preset '(topic context)))
                               clatter--topic)
                          (truncate-string-to-width clatter--topic 40 nil nil "...")
                        ""))
           (parts (delq nil
                        (list (and show-identity
                                   (format "[%s/%s]"
                                           clatter--network
                                           (or clatter--target "")))
                              (if (and nick-str away-str)
                                  (concat nick-str away-str)
                                (or nick-str away-str))
                              (and show-nicks
                                   (> nicks 0)
                                   (format "(%d)" nicks))
                              (clatter--mode-line-state-fragment conn))))
           (base (string-join parts " ")))
      (format " %s%s"
              base
              (if (> (length topic-str) 0)
                  (format " - %s" topic-str)
                "")))))

;; --- Hook into clatter-mode ---

(defun clatter-ui-setup-buffer (buffer)
  "Set up UI elements for a new clatter BUFFER."
  (with-current-buffer buffer
    ;; Seed the buffer-local invisibility spec from the global default.
    ;; Use a fresh copy so per-buffer /suppress and /unsuppress edits
    ;; never mutate the shared clatter-suppress-messages list.
    (setq buffer-invisibility-spec (copy-sequence clatter-suppress-messages))
    ;; Smart filtering uses the `noise' category.  Seed it automatically
    ;; only when smart filtering is enabled and has message types to filter;
    ;; an explicitly configured `noise' suppression remains untouched.
    (when (and clatter-smart-enabled clatter-smart-noise)
      (add-to-invisibility-spec 'noise))
    (unless clatter-fools-visible
      (add-to-invisibility-spec 'clatter-fool))
    (add-hook 'visible-mode-hook
              #'clatter--refresh-compact-system-layout nil t)
    (clatter--setup-prompt buffer)
    (clatter--timestamp-divider-seed buffer)
    ;; Add mode-line.  Optionally include the activity crumbs (see
    ;; `clatter-track-show-in-clatter-buffers') so they are visible while
    ;; inside a clatter buffer, not just in the global mode line.
    (setq-local mode-line-format
                (append
                 (unless (eq (clatter--effective-header-line-preset) 'context)
                   (list " " 'mode-line-buffer-identification))
                 (list clatter-mode-line-format)
                 (when (eq clatter-typing-indicator-location 'mode-line)
                   (list '(:eval (clatter--typing-mode-line))))
                 (when (and (boundp 'clatter-track-show-in-clatter-buffers)
                            clatter-track-show-in-clatter-buffers)
                   (list 'clatter-track-mode-line-item))
                 (list " " 'mode-line-end-spaces)))
    (setq-local header-line-format (clatter--effective-header-line-format))
    ;; Ensure window margins are synced for timestamp display
    (add-hook 'window-configuration-change-hook
              #'clatter--sync-window-margins nil t)
    (add-hook 'window-configuration-change-hook
              #'clatter--refresh-input-spacers 20 t)
    (add-hook 'post-command-hook #'clatter--refresh-input-spacers 90 t)
    ;; Outbound typing notifications
    (clatter--setup-outbound-typing buffer)
    (clatter--refresh-input-spacers buffer)))

(defun clatter--sync-window-margins ()
  "Ensure the current window has correct margins for timestamp display.
Emacs requires `set-window-margins' on the window, not just
the buffer margin-width variables."
  (when (and (derived-mode-p 'clatter-mode)
             (eq (current-buffer) (window-buffer)))
    (let ((ts-width (1+ (length (format-time-string clatter-timestamp-format)))))
      (pcase clatter-timestamp-side
        ('left
         (set-window-margins (selected-window) ts-width 0))
        ('right
         (set-window-margins (selected-window) 0 ts-width))
        (_
         (set-window-margins (selected-window) 0 0))))))

;; --- Nick completion ---

(defun clatter-complete-nick ()
  "Complete nick at point using the channel's nick list."
  (interactive)
  (when clatter--nick-list
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "^ \t\n")
                    (point)))
           (prefix (buffer-substring-no-properties start end))
           (nicks (cl-loop for k being the hash-keys of clatter--nick-list
                           using (hash-values prefix-and-nick)
                           when (string-prefix-p (downcase prefix) k)
                           collect (cdr prefix-and-nick))))
      (cond
       ((null nicks)
        (message "No matching nicks"))
       ((= (length nicks) 1)
        (delete-region start end)
        (insert (car nicks)
                (if (= start (marker-position clatter--input-marker)) ": " " ")))
       (t
        (let ((common (try-completion prefix
                                      (mapcar #'list nicks))))
          (when (stringp common)
            (delete-region start end)
            (insert common))
          (message "Nicks: %s" (string-join nicks ", "))))))))

;; --- Wire up event hooks ---

(defun clatter-ui--display-received-query (buf)
  "Display received query BUF according to `clatter-receive-query-display'."
  (pcase clatter-receive-query-display
    ('buffer (display-buffer buf))
    ('pop (pop-to-buffer buf))))

(defun clatter-ui--on-privmsg (conn sender target text server-time)
  "Display SENDER's PRIVMSG TEXT to TARGET on CONN at SERVER-TIME."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (isupport (clatter-connection-isupport conn))
         (case-mapping (and isupport (gethash "CASEMAPPING" isupport)))
         (sender-nick (clatter-prefix-nick sender))
         (buf-target (if (clatter-channel-name-p target)
                         target
                       (if (clatter-nick-equal-p target my-nick case-mapping)
                           sender-nick target)))
         (buf (clatter-get-or-create-buffer network buf-target))
         (is-muted (clatter-muted-p sender network))
         (invisible (clatter-sender-invisibility sender network)))
    (clatter-ui-setup-buffer-if-needed buf)
    (unless (and (member "echo-message" (clatter-connection-cap-enabled conn))
                 (clatter-nick-equal-p sender-nick my-nick case-mapping)
                 (clatter-ui--reconcile-self-echo buf sender-nick buf-target text 'privmsg server-time))
      (clatter-insert-privmsg buf sender-nick text conn server-time invisible))
    (when (and (not (clatter-channel-name-p target))
               (clatter-nick-equal-p target my-nick case-mapping)
               (not (clatter-nick-equal-p sender-nick my-nick case-mapping)))
      (clatter-ui--display-received-query buf))
    (when (and (not is-muted)
               (eq 'channel (buffer-local-value 'clatter--buffer-type buf))
               (not (string-equal-ignore-case my-nick sender-nick))
               (listp (buffer-local-value 'buffer-invisibility-spec buf))
               (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf)))
      (clatter-smart-put buf sender-nick 'privmsg))))

(defun clatter-ui--on-action (conn sender target text server-time)
  "Display SENDER's ACTION TEXT to TARGET on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (isupport (clatter-connection-isupport conn))
         (case-mapping (and isupport (gethash "CASEMAPPING" isupport)))
         (sender-nick (clatter-prefix-nick sender))
         (buf-target (if (clatter-channel-name-p target)
                         target
                       (if (clatter-nick-equal-p target my-nick case-mapping)
                           sender-nick target)))
         (buf (clatter-get-or-create-buffer network buf-target))
         (is-muted (clatter-muted-p sender network))
         (invisible (clatter-sender-invisibility sender network)))
    (clatter-ui-setup-buffer-if-needed buf)
    (unless (and (member "echo-message" (clatter-connection-cap-enabled conn))
                 (clatter-nick-equal-p sender-nick my-nick case-mapping)
                 (clatter-ui--reconcile-self-echo buf sender-nick buf-target text 'action server-time))
      (clatter-insert-action buf sender-nick text conn server-time invisible))
    (when (and (not (clatter-channel-name-p target))
               (clatter-nick-equal-p target my-nick case-mapping)
               (not (clatter-nick-equal-p sender-nick my-nick case-mapping)))
      (clatter-ui--display-received-query buf))
    (when (and (not is-muted)
               (eq 'channel (buffer-local-value 'clatter--buffer-type buf))
               (not (string-equal-ignore-case my-nick sender-nick))
               (listp (buffer-local-value 'buffer-invisibility-spec buf))
               (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf)))
      (clatter-smart-put buf sender-nick 'privmsg))))

(defun clatter-ui--on-notice (conn sender target text server-time)
  "Display SENDER's NOTICE TEXT to TARGET on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (isupport (clatter-connection-isupport conn))
         (case-mapping (and isupport (gethash "CASEMAPPING" isupport)))
         (real-sender-nick (clatter-prefix-nick sender))
         (sender-nick (or real-sender-nick "*"))
         ;; A NOTICE addressed to our own nick from a real user (a
         ;; nick!user@host prefix, not a bare server name) is a query NOTICE:
         ;; route it to (or create) the query buffer keyed by the sender's
         ;; nick, mirroring `clatter-ui--on-privmsg'.  Server notices (a
         ;; servername prefix) and channel notices keep the legacy
         ;; server-buffer/channel-buffer fallback.
         (user-prefix-p (and (cadr sender) (caddr sender)))
         (to-me (and (not (clatter-channel-name-p target))
                     user-prefix-p
                     (clatter-nick-equal-p target my-nick case-mapping)))
         (buf (if to-me
                  (clatter-get-or-create-buffer network real-sender-nick)
                (or (clatter-get-buffer network target)
                    (clatter-get-server-buffer network)
                    (clatter-get-or-create-buffer network "*server*" 'server))))
         (is-muted (clatter-muted-p sender network))
         (invisible (clatter-sender-invisibility sender network)))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter-insert-notice buf sender-nick text conn server-time invisible)
    (when (and (not is-muted)
               (not (string-equal-ignore-case my-nick sender-nick))
               (eq 'channel (buffer-local-value 'clatter--buffer-type buf))
               (listp (buffer-local-value 'buffer-invisibility-spec buf))
               (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf)))
      (clatter-smart-put buf sender-nick 'notice))))

(defun clatter-ui--on-invite (conn sender nick channel)
  "Show that SENDER invited NICK to CHANNEL on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (sender-nick (clatter-prefix-nick sender))
         (buf (or (clatter-get-buffer network channel)
                  (clatter-get-server-buffer network)
                  (clatter-get-or-create-buffer network "*server*" 'server)))
         (invisible (clatter-sender-invisibility sender network)))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter--insert-system-event
     buf 'invite
     (list :nick sender-nick
           :invitee nick
           :channel channel
           :verbose (format "%s invites %s to join %s"
                            sender-nick
                            (if (string-equal nick my-nick) "you" nick)
                            channel))
     (if invisible (list 'invite invisible) 'invite))))

(defun clatter-ui--on-join (conn sender channel _account realname)
  "Show SENDER joining CHANNEL on CONN, noting REALNAME when present."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (sender-nick (clatter-prefix-nick sender))
         (buf (clatter-get-or-create-buffer network channel))
         (is-muted (clatter-muted-p sender network))
         (invisible (clatter-sender-invisibility sender network)))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter-nick-add buf sender-nick)
    (when (string-equal sender-nick my-nick)
      (clatter-send conn (clatter-irc-names channel))
      (when clatter-display-on-join
        (display-buffer buf)))
    (let ((parsed-realname (and realname (clatter-format-parse realname))))
      (clatter--insert-system-event
       buf 'join
       (list :nick sender-nick
             :channel channel
             :realname parsed-realname
             :verbose (if (and realname (not (string= sender-nick realname)))
                          (format "%s (%s) has joined %s"
                                  sender-nick parsed-realname channel)
                        (format "%s has joined %s" sender-nick channel)))
       (append (if invisible (list 'join invisible) '(join))
               (and (not is-muted)
                    (not (string-equal-ignore-case my-nick sender-nick))
                    (listp (buffer-local-value 'buffer-invisibility-spec buf))
                    (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf))
                    (clatter-smart-eval buf sender-nick 'join)
                    '(noise)))))))

(defun clatter-ui--on-part (conn sender channel message)
  "Show SENDER leaving CHANNEL on CONN with optional MESSAGE."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (sender-nick (clatter-prefix-nick sender))
         (buf (clatter-get-buffer network channel))
         (is-muted (clatter-muted-p sender network))
         (invisible (clatter-sender-invisibility sender network)))
    (when buf
      (clatter-nick-remove buf sender-nick)
      (let ((reason (and message (clatter-format-parse message))))
        (clatter--insert-system-event
         buf 'part
         (list :nick sender-nick
               :channel channel
               :reason reason
               :verbose (format "%s has left %s%s" sender-nick channel
                                (if message (format " (%s)" reason) "")))
         (append (if invisible (list 'part invisible) '(part))
                 (and (not is-muted)
                      (not (string-equal-ignore-case my-nick sender-nick))
                      (listp (buffer-local-value 'buffer-invisibility-spec buf))
                      (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf))
                      (clatter-smart-eval buf sender-nick 'part)
                      '(noise))))))))

(defun clatter-ui--on-quit (conn sender message)
  "Show SENDER quitting on CONN with optional MESSAGE."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (sender-nick (clatter-prefix-nick sender))
         (is-muted (clatter-muted-p sender network))
         (invisible (clatter-sender-invisibility sender network)))
    (dolist (buf (clatter-channel-buffers network))
      (when (gethash (downcase sender-nick)
                     (buffer-local-value 'clatter--nick-list buf))
        (clatter-nick-remove buf sender-nick)
        (let ((reason (and message (clatter-format-parse message))))
          (clatter--insert-system-event
           buf 'quit
           (list :nick sender-nick
                 :channel (buffer-local-value 'clatter--target buf)
                 :reason reason
                 :verbose (format "%s has quit%s" sender-nick
                                  (if message (format " (%s)" reason) "")))
           (append (if invisible (list 'quit invisible) '(quit))
                   (and (not is-muted)
                        (not (string-equal-ignore-case my-nick sender-nick))
                        (listp (buffer-local-value 'buffer-invisibility-spec buf))
                        (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf))
                        (clatter-smart-eval buf sender-nick 'quit)
                        '(noise)))))))))

(defun clatter-ui--on-nick (conn sender new-nick)
  "Show SENDER renaming to NEW-NICK on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (old-nick (clatter-prefix-nick sender))
         (is-muted (clatter-muted-p sender network))
         (invisible (clatter-sender-invisibility sender network)))
    (dolist (buf (clatter-channel-buffers network))
      (when (gethash (downcase old-nick)
                     (buffer-local-value 'clatter--nick-list buf))
        (clatter-nick-rename buf old-nick new-nick)
        (clatter--insert-system-event
         buf 'nick
         (list :nick old-nick
               :new-nick new-nick
               :channel (buffer-local-value 'clatter--target buf)
               :verbose (format "%s is now known as %s" old-nick new-nick))
         (append (if invisible (list 'nick invisible) '(nick))
                 (and (not is-muted)
                      (not (string-equal-ignore-case my-nick new-nick))
                      (listp (buffer-local-value 'buffer-invisibility-spec buf))
                      (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf))
                      (clatter-smart-eval buf old-nick new-nick)
                      '(noise))))))
    ;; The handler has already updated CONN's nick.  Refresh every prompt on
    ;; this network only when this was our own nick change and the configured
    ;; prompt may depend on the nick.
    (when (string-equal-ignore-case new-nick my-nick)
      (dolist (buf (clatter-all-buffers network))
        (with-current-buffer buf
          (when (clatter--prompt-format-needs-nick-p)
            (clatter--refresh-prompt)))))))

(defun clatter-ui--on-topic (conn channel sender topic at)
  "Show TOPIC for CHANNEL set by SENDER at AT on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (sender-nick (clatter-prefix-nick sender))
         (buf (clatter-get-buffer network channel)))
    (when buf
      (clatter-set-topic buf topic)
      (let ((prefix "Topic")
            (hl-text (clatter-hl-format-text (or topic "") buf conn)))
        (cond
         ((and sender-nick at)
          (setq prefix (format "%s set at %s by %s"
                               prefix
                               (format-time-string "%F %T" at)
                               sender-nick)))
         (sender-nick (setq prefix (format "%s set by %s" prefix sender-nick))))
        (clatter-insert-system buf (format "%s: %s" prefix hl-text) 'topic)
        (when (and (not (string-equal-ignore-case my-nick sender-nick))
                   ;; avoid recording nick!user@host from RPL_TOPICWHOTIME
                   (not at)
                   (listp (buffer-local-value 'buffer-invisibility-spec buf))
                   (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf)))
          (clatter-smart-put buf sender-nick 'topic))))))

(defun clatter-ui--on-kick (conn channel sender kicked reason)
  "Show NICK kicking KICKED from CHANNEL on CONN with REASON."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (sender-nick (clatter-prefix-nick sender))
         (buf (clatter-get-buffer network channel))
         (is-muted (clatter-muted-p sender network))
         (invisible (clatter-sender-invisibility sender network)))
    (when buf
      (clatter-nick-remove buf kicked)
      (let ((parsed-reason (and reason (clatter-format-parse reason))))
        (clatter--insert-system-event
         buf 'kick
         (list :nick kicked
               :setter sender-nick
               :channel channel
               :reason parsed-reason
               :verbose (format "%s was kicked by %s%s" kicked sender-nick
                                (if reason (format " (%s)" parsed-reason) "")))
         (append (if invisible (list 'kick invisible) '(kick))
                 (and (not is-muted)
                      (not (string-equal-ignore-case my-nick sender-nick))
                      (listp (buffer-local-value 'buffer-invisibility-spec buf))
                      (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf))
                      (clatter-smart-eval buf sender-nick 'kick)
                      '(noise))))))))

(defun clatter-ui--on-names (conn channel names-str)
  "Populate the CHANNEL nick list on CONN from NAMES-STR."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network channel))
         (prefixes (or (let ((isup (clatter-connection-isupport conn)))
                         (when isup
                           (let ((prefix (gethash "PREFIX" isup)))
                             (and prefix
                                  (string-match (rx bol ?\( (+ alpha) ?\) (group (+ anything)) eol)
                                                prefix)
                                  (match-string 1 prefix)))))
                       clatter-prefix-rank)))
    (when buf
      (dolist (entry (clatter-parse-names names-str prefixes))
        (clatter-nick-add buf (car entry) (cdr entry))))))

(defun clatter-ui--on-system (conn text)
  "Show system message TEXT on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (buf (or (clatter-get-server-buffer network)
                  (clatter-get-or-create-buffer network "*server*" 'server))))
    (when (buffer-live-p buf)
      (clatter-ui-setup-buffer-if-needed buf)
      (clatter-insert-system buf text))))

(defun clatter-ui--on-welcome (conn _nick)
  "Handle 001 welcome for UI on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-or-create-buffer network "*server*" 'server)))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter-insert-system buf
                           (format "Connected to %s as %s"
                                   network (clatter-connection-nick conn)))
    (when clatter-display-on-welcome
      (display-buffer buf))))

(defun clatter-ui-setup-buffer-if-needed (buf)
  "Set up UI for BUF if not already done."
  (with-current-buffer buf
    (unless clatter--prompt-marker
      (clatter-ui-setup-buffer buf))))

;; --- Register hooks ---

(defun clatter-ui--on-away (conn sender away-msg)
  "Show SENDER away state (AWAY-MSG) on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (sender-nick (clatter-prefix-nick sender))
         (is-muted (clatter-muted-p sender network))
         (invisible (clatter-sender-invisibility sender network)))
    (dolist (buf (clatter-channel-buffers network))
      (when (gethash (downcase sender-nick)
                     (buffer-local-value 'clatter--nick-list buf))
        (let ((reason (and away-msg (clatter-format-parse away-msg))))
          (clatter--insert-system-event
           buf (if away-msg 'away 'back)
           (list :nick sender-nick
                 :channel (buffer-local-value 'clatter--target buf)
                 :reason reason
                 :verbose (if away-msg
                              (format "%s is away: %s" sender-nick reason)
                            (format "%s is back" sender-nick)))
           (append (if invisible (list 'away invisible) '(away))
                   (and (not is-muted)
                        (not (string-equal-ignore-case my-nick sender-nick))
                        (listp (buffer-local-value 'buffer-invisibility-spec buf))
                        (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf))
                        (clatter-smart-eval buf sender-nick 'away)
                        '(noise)))))))))

(defun clatter-ui--on-mode (conn target setter modes)
  "Show SETTER applying MODES on TARGET on CONN."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (setter-nick (clatter-prefix-nick setter))
         (buf (or (clatter-get-buffer network target)
                  (clatter-get-server-buffer network)))
         (is-muted (clatter-muted-p setter network))
         (invisible (clatter-sender-invisibility setter network)))
    (when buf
      (let ((mode-string (string-join modes " ")))
        (clatter--insert-system-event
         buf 'mode
         (list :nick setter-nick
               :channel target
               :modes mode-string
               :verbose (format "%s sets mode %s" setter-nick mode-string))
         (append (if invisible (list 'mode invisible) '(mode))
                 (and (not is-muted)
                      (not (string-equal-ignore-case my-nick setter-nick))
                      (listp (buffer-local-value 'buffer-invisibility-spec buf))
                      (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf))
                      (clatter-smart-eval buf setter-nick 'mode)
                      '(noise))))))))

(defun clatter-ui--on-motd (conn lines)
  "Display MOTD LINES on CONN.
Routes to the buffer that requested the MOTD via /motd when live; the
server buffer otherwise (e.g. the connect-time MOTD)."
  (let* ((network (clatter-connection-network-id conn))
         (query-buf (clatter--query-target conn))
         (buf (or query-buf
                  (clatter-get-server-buffer network)
                  (clatter-get-or-create-buffer network "*server*" 'server))))
    (clatter-ui-setup-buffer-if-needed buf)
    (clatter--insert-message buf (clatter--divider "MOTD") t)
    (dolist (line lines)
      (clatter-insert-system buf (clatter-hl-urls-in-string (clatter-format-parse line)) nil))
    (clatter--insert-message buf (clatter--divider "End of MOTD") t)))

(defun clatter-ui--on-whois (conn nick data)
  "Handle WHOIS reply for UI: display NICK info from DATA.
Routes to the buffer that issued /whois when live; the server buffer
otherwise (the process filter may run in any buffer, so don't rely on
`current-buffer')."
  (let* ((network (clatter-connection-network-id conn))
         (buf (or (clatter--query-target conn)
                  (clatter-get-server-buffer network)
                  (current-buffer)))
         (parts nil))
    (push (format "WHOIS %s (%s@%s)"
                  nick
                  (or (plist-get data :user) "?")
                  (or (plist-get data :host) "?"))
          parts)
    (when (plist-get data :realname)
      (push (format "  Realname: %s" (clatter-format-parse (plist-get data :realname))) parts))
    (when (plist-get data :account)
      (push (format "  Account: %s" (plist-get data :account)) parts))
    (when (plist-get data :regnick)
      (push (format "  Registered: %s" (plist-get data :regnick)) parts))
    (when (plist-get data :modes)
      (push (format "  Modes: %s" (plist-get data :modes)) parts))
    (when (plist-get data :server)
      (push (format "  Server: %s (%s)"
                    (plist-get data :server)
                    (or (plist-get data :server-info) "")) parts))
    (when (plist-get data :conn)
      (push (format "  Host: %s" (plist-get data :conn)) parts))
    (when (plist-get data :actually)
      (push (format "  Details: %s" (plist-get data :actually)) parts))
    (when (plist-get data :channels)
      (push (format "  Channels: %s" (plist-get data :channels)) parts))
    (when (plist-get data :idle)
      (let* ((idle-secs (string-to-number (plist-get data :idle)))
             (idle-str (cond
                        ((< idle-secs 60) (format "%ds" idle-secs))
                        ((< idle-secs 3600) (format "%dm" (/ idle-secs 60)))
                        (t (format "%dh %dm" (/ idle-secs 3600)
                                   (/ (mod idle-secs 3600) 60))))))
        (push (format "  Idle: %s" idle-str) parts)))
    (when (plist-get data :signon)
      (let ((time (seconds-to-time
                   (string-to-number (plist-get data :signon)))))
        (push (format "  Signon: %s" (format-time-string "%F %T" time)) parts)))
    (when (plist-get data :secure)
      (push "  Secure connection (TLS)" parts))
    (when (plist-get data :certfp)
      (push (format "  Fingerprint: %s" (plist-get data :certfp)) parts))
    (when (plist-get data :oper)
      (push "  IRC Operator" parts))
    (when (plist-get data :away)
      (push (format "  Away: %s" (plist-get data :away)) parts))
    (when (plist-get data :bot)
      (push "  Is a bot." parts))
    (when (plist-get data :special)
      (push (format "  Notes: %s" (plist-get data :special)) parts))
    (dolist (line (nreverse parts))
      (clatter-insert-system buf line))))

(defun clatter-ui--on-disconnect (network-id event)
  "Handle disconnect EVENT for UI: show message in all NETWORK-ID buffers."
  (clatter-ui--clear-pending-self-echoes network-id)
  (dolist (buf (clatter-all-buffers network-id))
    (when (buffer-live-p buf)
      (clatter-insert-error buf
                            (format "Disconnected: %s" (string-trim event))))))

(defun clatter-ui--on-reconnect (network-id delay attempt)
  "Handle reconnect scheduling (DELAY, ATTEMPT) for UI in NETWORK-ID buffers."
  (dolist (buf (clatter-all-buffers network-id))
    (clatter-insert-system buf
                           (format "Reconnecting in %ds (attempt %d)..."
                                   delay attempt))))

(defun clatter-ui--on-react (conn sender target emoji msgid)
  "Handle reaction on CONN: display EMOJI from NICK on message MSGID in TARGET."
  (let* ((network (clatter-connection-network-id conn))
         (my-nick (clatter-connection-nick conn))
         (sender-nick (clatter-prefix-nick sender))
         (buf (clatter-get-buffer network target))
         (is-muted (clatter-muted-p sender network)))
    (when (and buf (buffer-live-p buf))
      (when (and (not is-muted)
                 (eq 'channel (buffer-local-value 'clatter--buffer-type buf))
                 (not (string-equal-ignore-case my-nick sender-nick))
                 (listp (buffer-local-value 'buffer-invisibility-spec buf))
                 (memq 'noise (buffer-local-value 'buffer-invisibility-spec buf)))
        (clatter-smart-put buf sender-nick 'react))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (when-let* ((found (clatter--find-message-position-by-msgid buf msgid))
                      (change (or (next-single-property-change found 'clatter-msgid)
                                  (point-max))))
            (setq found (- change 2))
            (goto-char found)
            (let ((inhibit-read-only t)
                  (existing (get-text-property found 'clatter-reactions)))
              (unless existing (setq existing nil))
              ;; Add this reaction, with an indicator prefix.
              ;; Prefix with - for reactions from muted senders.
              ;; Prefix with + for reactions from non-muted senders.
              (let* ((key (if is-muted (format "-%s" emoji)
                            (format "+%s" emoji)))
                     (entry (assoc key existing))
                     (new-reactions
                      (if entry
                          (progn (setcdr entry (cons sender-nick (cdr entry)))
                                 existing)
                        (append existing (list (list key sender-nick)))))
                     (display (mapconcat
                               (lambda (r)
                                 (let* ((label (car r))
                                        (entries (cdr r))
                                        (count (length entries))
                                        (indicator (aref label 0))
                                        (muted (eq ?- indicator)))
                                   (setq label (substring label 1))
                                   (let ((formatted (format "%s %d" label count)))
                                     (when muted
                                       (add-face-text-property 0 (length formatted)
                                                               'clatter-muted-reaction nil
                                                               formatted))
                                     formatted)))
                               new-reactions " ")))
                ;; Remove old reaction overlay if any
                (dolist (ov (overlays-at found))
                  (when (overlay-get ov 'clatter-reaction)
                    (delete-overlay ov)))
                ;; Add new overlay showing reactions
                (let ((ov (make-overlay (line-beginning-position)
                                        (line-end-position))))
                  (overlay-put ov 'clatter-reaction t)
                  (add-face-text-property 0 (length display) 'clatter-reaction nil display)
                  (overlay-put ov 'after-string
                               (concat "\n"
                                       (make-string clatter-nick-column-width ?\s)
                                       " " display)))
                ;; Store reactions as property
                (add-text-properties found (1+ found)
                                     (list 'clatter-reactions new-reactions))))))))))

(defun clatter--divider (label)
  "Return a window-wide divider line with LABEL centered.
The rule segments are stretch glyphs drawn with `:strike-through'.
Redisplay recenters and resizes the bar with each window, without
width calculations, overlays, or resize hooks."
  (let ((rule '(:inherit clatter-divider :strike-through t :extend t))
        (label (concat " " label " ")))
    (concat
     (propertize " " 'face rule
                 'display `(space :align-to (- center ,(/ (string-width label) 2))))
     (propertize label 'face 'clatter-divider)
     ;; Non-whitespace backing character survives `fill-region'.
     (propertize "-" 'face rule 'display '(space :align-to right)))))

(defun clatter-ui--on-batch-complete (conn _batch-type target messages)
  "Handle completed batch: render MESSAGES for TARGET on CONN.
Renders a visual separator before and after history playback.
A page requested by `clatter-chathistory-more' is older than everything
on screen, so it renders at the buffer's oldest end, in the direction
`clatter-message-order' dictates."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network target)))
    (when (and buf (buffer-live-p buf) messages)
      ;; Insert separator before history.  Read-state handling belongs to the
      ;; insertion path so seen messages remain visible without adding activity.
      (let* ((backlog (with-current-buffer buf
                        (prog1 clatter--backlog-page-pending
                          (setq-local clatter--backlog-page-pending nil))))
             (clatter--insert-at-backlog-end backlog)
             (count (length messages))
             ;; Rendering at the oldest end walks the block backwards: the
             ;; end separator, then messages newest-first, then the start
             ;; separator.
             (messages (if backlog (reverse messages) messages))
             (sep-text (clatter--divider "history"))
             (end-sep-text (clatter--divider
                            (format "end of history (%d messages)" count))))
        (clatter--insert-message buf (if backlog end-sep-text sep-text) t)
        ;; Insert each message with dimmed style.  Suppress inline image
        ;; scanning/fetching for history playback: a large backlog would
        ;; otherwise scan every old message and stampede curl subprocesses.
        (let ((clatter--suppress-image-scan t)
              ;; History replays messages with their original, often widely
              ;; spaced server-times, so the live burst window would split a
              ;; visual burst and re-show the nick on every line.  Group by
              ;; adjacency instead.
              (clatter--group-messages-adjacency-only t))
          (dolist (msg messages)
            (let* ((msg-type (plist-get msg :type))
                   (sender (plist-get msg :sender))
                   (text (plist-get msg :text))
                   (time (plist-get msg :time))
                   ;; Batch messages carry only the nick, not a full
                   ;; nick!user@host prefix, so wrap it for
                   ;; `clatter-sender-invisibility'.  This makes fools
                   ;; (and ignored senders) in playback toggleable just
                   ;; like live buffer messages.
                   (invisible (clatter-sender-invisibility
                               (list sender nil nil) network)))
              (pcase msg-type
                ('action (clatter-insert-action buf sender text conn time invisible))
                (_ (clatter-insert-privmsg buf sender text conn time invisible))))))
        ;; Insert end separator
        (clatter--insert-message buf (if backlog sep-text end-sep-text) t)))))

;; --- CTCP replies ---

(defun clatter-ui--on-ctcp-reply (conn sender command reply-text)
  "Display CTCP reply from SENDER on CONN in the current clatter buffer.
COMMAND is the CTCP type (VERSION, PING, etc.), REPLY-TEXT is the response."
  (let* ((network (clatter-connection-network-id conn))
         (sender-nick (clatter-prefix-nick sender))
         (buf (or (clatter-get-buffer network sender-nick)
                  (when-let* ((win (selected-window)))
                    (with-current-buffer (window-buffer win)
                      (when (derived-mode-p 'clatter-mode)
                        (current-buffer))))
                  (clatter-get-server-buffer network))))
    (when (and buf (buffer-live-p buf))
      (clatter-insert-system
       buf (format "CTCP %s reply from %s: %s" command sender-nick reply-text)))))

;; --- Handlers for other numerics ---

(defconst clatter--query-reply-numerics
  '("303"  ; RPL_ISON
    "314"  ; RPL_WHOWASUSER
    "315"  ; RPL_ENDOFWHO
    "351"  ; RPL_VERSION
    "352"  ; RPL_WHOREPLY
    "364"  ; RPL_LINKS
    "365"  ; RPL_ENDOFLINKS
    "369"  ; RPL_ENDOFWHOWAS
    "371"  ; RPL_INFO
    "374"  ; RPL_ENDOFINFO
    "210" "211" "212" "213" "214" "215" "216" "217" "218" "219" "220"
    "240" "241" "242" "243" "244" "245" "246" "247" "248" "249" "250"
    "256" "257" "258" "259"               ; RPL_ADMIN*
    "251" "252" "253" "254" "255" "265" "266")  ; RPL_LUSER* (also welcome burst)
  "Numeric replies that belong to user-initiated query commands.
When `clatter-connection-last-query-buffer' is live, these route to that
buffer instead of the server buffer.  The welcome burst sends several of
these (251-266) too, but `last-query-buffer' is nil then so they still go
to the server buffer.")

(defconst clatter--query-end-numerics
  '("219"  ; RPL_ENDOFSTATS
    "315"  ; RPL_ENDOFWHO
    "365"  ; RPL_ENDOFLINKS
    "369"  ; RPL_ENDOFWHOWAS
    "374"  ; RPL_ENDOFINFO
    "376" "422")  ; RPL_ENDOFMOTD, ERR_NOMOTD
  "Numerics that terminate a query reply set.
Receiving one clears `clatter-connection-last-query-buffer' so that
later unsolicited replies of the same kind (e.g. the automatic WHO issued
on rejoin) fall back to the server buffer instead of being routed into
whichever buffer last issued a query command.")

(defun clatter--query-target (conn)
  "Return the live buffer to route query replies to on CONN, or nil.
Falls back to nil so callers use the normal server-buffer routing."
  (let ((buf (clatter-connection-last-query-buffer conn)))
    (and buf (buffer-live-p buf) buf)))

(defun clatter--internal-whox-reply-p (conn command params)
  "Return non-nil if COMMAND/PARAMS is a reply to an internal WHOX on CONN.
`clatter-send-whox' fires automatically for every channel on each
RPL_ENDOFNAMES, so its 352/315 replies must be consumed silently instead
of flooding a buffer.  Clears the pending entry on the terminating 315."
  (let ((channel (nth 1 params)))
    (and channel
         (member (downcase channel) (clatter-connection-pending-whox conn))
         (cond
          ;; 315 terminates the reply set: consume it and forget the channel.
          ((string= command "315")
           (setf (clatter-connection-pending-whox conn)
                 (delete (downcase channel)
                         (clatter-connection-pending-whox conn)))
           t)
          ;; 352/354 bodies for that same in-flight query.
          ((member command '("352" "354")) t)))))

(defun clatter-ui--on-numeric (conn command params)
  "Handle informational and MODE-related numerics for UI.
COMMAND is the numeric reply code, PARAMS its parameters on CONN."
  (cl-block clatter-ui--on-numeric
    ;; Route replies to user-initiated query commands (/who, /stats,
    ;; /lusers, ...) to the buffer the command was typed in, when it is
    ;; still live.  Falls through to the pcase below otherwise (e.g. the
    ;; welcome burst, where `last-query-buffer' is nil).
    ;; Replies to the automatic per-channel WHOX are internal bookkeeping
    ;; (account names); never display them.
    (when (clatter--internal-whox-reply-p conn command params)
      (cl-return-from clatter-ui--on-numeric))
    (let ((query-buf (clatter--query-target conn)))
      (when (and query-buf
                 (member command clatter--query-reply-numerics))
        ;; A terminating numeric ends the query: stop routing so later
        ;; unsolicited replies go to the server buffer instead of piling up
        ;; in whatever buffer last ran a query command.
        (when (member command clatter--query-end-numerics)
          (setf (clatter-connection-last-query-buffer conn) nil))
        (clatter-insert-system
         query-buf (format "[%s] %s" command (string-join (cdr params) " ")))
        (cl-return-from clatter-ui--on-numeric)))
    (pcase command
      ;; --- Informational numerics ---
      ((or "001" "002" "003" "004" "242" "251" "252" "253" "254" "255"
           "265" "266")
       (let* ((network (clatter-connection-network-id conn))
              (buf (clatter-get-server-buffer network)))
         (when buf
           (clatter-insert-system buf (string-join (cdr params) " ")))))
      ((or "305" "306") ;  RPL_UNAWAY, RPL_NOWAWAY
       (let* ((network (clatter-connection-network-id conn))
              (buf (clatter-get-server-buffer network))
              (msg (string-join (cdr params) " ")))
         (when buf
           (clatter-insert-system buf msg))
         (dolist (buf (clatter-channel-buffers network))
           (clatter-insert-system buf msg)))
       (when (clatter--prompt-format-needs-away-p)
         (clatter--refresh-prompt)))
      ;; --- MODE numerics ---
      ("221"   ; RPL_UMODEIS
       (let* ((network (clatter-connection-network-id conn))
              (buf (clatter-get-server-buffer network))
              (nick (nth 0 params))
              (modes (nth 1 params)))
         (when buf
           (clatter-insert-system buf (format "%s is %s" nick modes)))))
      ("324"   ; RPL_CHANNELMODEIS
       (let* ((network (clatter-connection-network-id conn))
              (channel (nth 1 params))
              (buf (clatter-get-buffer network channel))
              (modes (nth 2 params)))
         (when buf
           (clatter-set-channel-modes buf modes)
           (clatter-insert-system buf (format "%s is %s" channel modes)))))
      ("329"   ; RPL_CREATIONTIME
       (let* ((network (clatter-connection-network-id conn))
              (channel (nth 1 params))
              (buf (clatter-get-buffer network channel))
              (ctime (string-to-number (nth 2 params))))
         (when buf
           (clatter-insert-system
            buf (format "%s was created at %s"
                        channel (format-time-string "%F %T" ctime))))))
      ((or "401" "403"  ; ERR_NOSUCHNICK, ERR_NOSUCHCHANNEL
           "404"        ; ERR_CANNOTSENDTOCHAN
           "475")       ; ERR_BADCHANNELKEY
       ;; Route to the buffer named by the reply's target param (a query
       ;; buffer for 401's nick, a channel buffer for the others), falling
       ;; back to the server buffer.  Do not use (current-buffer): the
       ;; process filter may run in any buffer.
       (let* ((network (clatter-connection-network-id conn))
              (target (nth 1 params))
              (buf (or (clatter-get-buffer network target)
                       (clatter-get-server-buffer network))))
         (when buf
           (clatter-insert-system buf (string-join (reverse (cdr params)) " ")))))
      ("421"                            ; ERR_UNKNOWNCOMMAND
       ;; The server rejected a command we sent.  If it was TAGMSG, the
       ;; server (or an upstream behind a bouncer/bridge) ACKed message-tags
       ;; but does not actually accept TAGMSG; remember that so outbound
       ;; typing/reaction TAGMSGs stop rather than spamming 421s every
       ;; keystroke.  Still display the error so the user sees it once.
       (let ((rejected-cmd (nth 1 params)))
         (when (string-equal (and rejected-cmd (upcase rejected-cmd)) "TAGMSG")
           (setf (clatter-connection-tagmsg-rejected conn) t)))
       (let* ((network (clatter-connection-network-id conn))
              (buf (clatter-get-server-buffer network)))
         (when buf
           (clatter-insert-system
            buf (format "[%s] %s" command (string-join (cdr params) " "))))))
      ;; Catch-all: show any unhandled numeric (e.g. 421 ERR_UNKNOWNCOMMAND,
      ;; 411/412/432/433/461/471-477 ...) in the server buffer instead of
      ;; silently dropping it.
      (_
       (let* ((network (clatter-connection-network-id conn))
              (buf (clatter-get-server-buffer network)))
         (when buf
           (clatter-insert-system
            buf (format "[%s] %s" command (string-join (cdr params) " "))))))))
  ) ;; end of pcase / cl-block clatter-ui--on-numeric

;; --- Channel preview on hover (eldoc) ---

(defun clatter-ui--eldoc-function (callback &rest _)
  "Eldoc function for clatter buffers, returning info via CALLBACK.
Shows channel topic and user count when point is on a #channel name.
Shows sender info when point is on a message."
  (let ((channel (clatter-ui--channel-at-point))
        (sender (get-text-property (point) 'clatter-sender))
        (msgid (get-text-property (point) 'clatter-msgid)))
    (cond
     ;; Channel name at point
     (channel
      (let* ((network clatter--network)
             (buf (clatter-get-buffer network channel)))
        (when (and buf (buffer-live-p buf))
          (let ((topic (buffer-local-value 'clatter--topic buf))
                (nick-list (buffer-local-value 'clatter--nick-list buf)))
            (let ((parts nil))
              (when (and nick-list (hash-table-p nick-list))
                (push (format "%d users" (hash-table-count nick-list)) parts))
              (when topic
                (push (if (> (length topic) 60)
                          (concat (substring topic 0 57) "...")
                        topic)
                      parts))
              (when parts
                (funcall callback
                         (mapconcat #'identity parts " - ")
                         :thing channel
                         :face 'clatter-notice)))))))
     ;; Message at point - show sender and msgid
     (sender
      (funcall callback
               (concat sender
                       (when msgid (format "  [msgid: %s]" msgid)))
               :thing "message"
               :face 'font-lock-doc-face)))
    nil))

(defun clatter-ui--channel-at-point ()
  "Return channel name at point, or nil.
Scans around point for a channel name starting with #, &, !, or +."
  (save-excursion
    (let ((orig (point)))
      ;; Move backward over valid channel-name chars
      (skip-chars-backward "a-zA-Z0-9_#&!+\\-\\[\\]\\\\`^{}|.")
      ;; Check if we're now on a channel prefix
      (when (memq (char-after) '(?# ?& ?! ?+))
        (let ((start (point)))
          (forward-char 1)
          (skip-chars-forward "a-zA-Z0-9_\\-\\[\\]\\\\`^{}|.")
          ;; Only return if original point was within the channel name
          (when (and (> (point) start)
                     (>= (point) orig)
                     (<= start orig))
            (buffer-substring-no-properties start (point))))))))

(defun clatter-ui--setup-eldoc ()
  "Set up eldoc for clatter buffers."
  (require 'eldoc)
  (when (boundp 'eldoc-documentation-functions)
    (add-hook 'eldoc-documentation-functions
              #'clatter-ui--eldoc-function nil t)
    (eldoc-mode 1)))

;; --- Typing indicators ---

(defcustom clatter-typing-timeout 6
  "Seconds after which a typing indicator expires.
If no update is received within this time, the indicator is cleared."
  :type 'integer
  :group 'clatter)

(defvar-local clatter--typing-nicks nil
  "Hash table of nicks currently typing in this buffer.
Keys are nick strings, values are timer objects.")

(defun clatter--refresh-typing-indicator ()
  "Refresh typing status in the current buffer."
  (when (overlayp clatter--typing-indicator-overlay)
    (overlay-put
     clatter--typing-indicator-overlay 'before-string
     (when-let* ((text (clatter--typing-string)))
       (concat (make-string (1+ clatter-nick-column-width) ?\s) text))))
  (force-mode-line-update))

(defun clatter--expire-typing-indicator (buffer nick)
  "Remove NICK's expired typing indicator from BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when clatter--typing-nicks
        (remhash nick clatter--typing-nicks))
      (clatter--refresh-typing-indicator))))

(defun clatter-ui--on-typing (conn sender target state)
  "Handle typing indicator from NICK in TARGET with STATE on CONN.
STATE is \"active\", \"paused\", or \"done\"."
  (let* ((network (clatter-connection-network-id conn))
         (buf (clatter-get-buffer network target))
         (nick (clatter-prefix-nick sender)))
    (when (and buf (buffer-live-p buf) (not (clatter-muted-p sender network)))
      (with-current-buffer buf
        (unless clatter--typing-nicks
          (setq clatter--typing-nicks (make-hash-table :test 'equal)))
        ;; Cancel any existing timer for this nick
        (let ((existing-timer (gethash nick clatter--typing-nicks)))
          (when (timerp existing-timer)
            (cancel-timer existing-timer)))
        (if (string-equal state "done")
            ;; Remove typing indicator
            (remhash nick clatter--typing-nicks)
          ;; Set typing indicator with auto-expiry
          (let ((the-buf (current-buffer))
                (the-nick nick))
            (puthash nick
                     (run-at-time clatter-typing-timeout nil
                                  #'clatter--expire-typing-indicator
                                  the-buf the-nick)
                     clatter--typing-nicks)))
        (clatter--refresh-typing-indicator)))))

(defun clatter--typing-string ()
  "Return propertized typing status text, or nil."
  (when (and clatter--typing-nicks
             (> (hash-table-count clatter--typing-nicks) 0))
    (let ((nicks nil))
      (maphash (lambda (k _v) (push k nicks)) clatter--typing-nicks)
      (propertize
       (concat (pcase (length nicks)
                 (1 (format "%s is typing" (car nicks)))
                 (2 (format "%s and %s are typing" (car nicks) (cadr nicks)))
                 (_ (format "%d people typing" (length nicks))))
               "...")
       'face 'font-lock-doc-face))))

(defun clatter--typing-mode-line ()
  "Return a mode-line string showing who is typing, or nil."
  (when-let* ((text (clatter--typing-string)))
    (concat " " text)))

;; --- Outbound typing notifications ---

(defcustom clatter-send-typing t
  "Whether to send typing indicators to the server.
Requires the server to support the message-tags capability."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-typing-throttle 3
  "Minimum seconds between outbound typing notifications."
  :type 'number
  :group 'clatter)

(defvar-local clatter--typing-last-sent nil
  "Time (float) when the last typing notification was sent.")

(defun clatter--typing-capable-p ()
  "Return non-nil if we can send typing notifications."
  (and clatter-send-typing
       clatter--network
       clatter--target
       (not (string= clatter--target "*server*"))
       (let ((conn (clatter-get-connection clatter--network)))
         (and conn
              (member "message-tags"
                      (clatter-connection-cap-enabled conn))
              (not (clatter-connection-tagmsg-rejected conn))))))

(defun clatter--maybe-send-typing (&rest _)
  "Send a typing notification if in the input area and throttle allows."
  (when (and clatter--input-marker
             (>= (point) (marker-position clatter--input-marker))
             (clatter--typing-capable-p))
    (let ((now (float-time)))
      (when (or (null clatter--typing-last-sent)
                (> (- now clatter--typing-last-sent) clatter-typing-throttle))
        (setq clatter--typing-last-sent now)
        (let ((conn (clatter-get-connection clatter--network)))
          (when conn
            (clatter-send conn
                          (clatter-irc-typing clatter--target "active"))))))))

(defun clatter--send-typing-done ()
  "Send typing=done notification after sending a message."
  (when (clatter--typing-capable-p)
    (let ((conn (clatter-get-connection clatter--network)))
      (when conn
        (clatter-send conn
                      (clatter-irc-typing clatter--target "done"))))
    (setq clatter--typing-last-sent nil)))

(defun clatter--setup-outbound-typing (buffer)
  "Set up outbound typing notifications for BUFFER."
  (with-current-buffer buffer
    (add-hook 'post-self-insert-hook #'clatter--maybe-send-typing nil t)))

(defun clatter-ui-init ()
  "Register UI hooks.  Call this after loading clatter."
  (add-hook 'clatter-privmsg-hook #'clatter-ui--on-privmsg)
  (add-hook 'clatter-action-hook #'clatter-ui--on-action)
  (add-hook 'clatter-notice-hook #'clatter-ui--on-notice)
  (add-hook 'clatter-invite-hook #'clatter-ui--on-invite)
  (add-hook 'clatter-join-hook #'clatter-ui--on-join)
  (add-hook 'clatter-part-hook #'clatter-ui--on-part)
  (add-hook 'clatter-quit-hook #'clatter-ui--on-quit)
  (add-hook 'clatter-nick-hook #'clatter-ui--on-nick)
  (add-hook 'clatter-topic-hook #'clatter-ui--on-topic)
  (add-hook 'clatter-kick-hook #'clatter-ui--on-kick)
  (add-hook 'clatter-away-hook #'clatter-ui--on-away)
  (add-hook 'clatter-irc-mode-hook #'clatter-ui--on-mode)
  (add-hook 'clatter-names-hook #'clatter-ui--on-names)
  (add-hook 'clatter-system-hook #'clatter-ui--on-system)
  (add-hook 'clatter-welcome-hook #'clatter-ui--on-welcome)
  (add-hook 'clatter-disconnect-hook #'clatter-ui--on-disconnect)
  (add-hook 'clatter-reconnect-hook #'clatter-ui--on-reconnect)
  (add-hook 'clatter-motd-hook #'clatter-ui--on-motd)
  (add-hook 'clatter-whois-hook #'clatter-ui--on-whois)
  (add-hook 'clatter-react-hook #'clatter-ui--on-react)
  (add-hook 'clatter-batch-complete-hook #'clatter-ui--on-batch-complete)
  (add-hook 'clatter-ctcp-reply-hook #'clatter-ui--on-ctcp-reply)
  (add-hook 'clatter-numeric-hook #'clatter-ui--on-numeric)
  (add-hook 'clatter-typing-hook #'clatter-ui--on-typing)
  (add-hook 'clatter-mode-hook #'clatter-ui--setup-eldoc)
  (add-hook 'clatter-mode-hook #'clatter-input-formatting-mode)
  ;; Key bindings for input
  (define-key clatter-mode-map (kbd "RET") #'clatter-send-input)
  (define-key clatter-mode-map (kbd "TAB") #'clatter-tab)
  (define-key clatter-mode-map (kbd "<backtab>") #'clatter-backtab)
  (define-key clatter-mode-map (kbd "M-p") #'clatter-set-prev-input)
  (define-key clatter-mode-map (kbd "M-n") #'clatter-set-next-input)
  (define-key clatter-mode-map (kbd "C-a") #'clatter-bol)
  (define-key clatter-mode-map (kbd "M-DEL") #'clatter-backward-kill-word)
  (define-key clatter-mode-map (kbd "<C-backspace>") #'clatter-backward-kill-word)
  (define-key clatter-mode-map (kbd "M-d") #'clatter-kill-word)
  (define-key clatter-mode-map [home] #'clatter-bol))

;; Auto-init when loaded
(clatter-ui-init)

(provide 'clatter-ui)

;;; clatter-ui.el ends here
