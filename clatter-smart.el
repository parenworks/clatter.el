;;; clatter-smart.el --- Smart noise suppression for clatter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT
;; URL: https://github.com/parenworks/clatter.el

;;; Commentary:

;; A simple signal-to-noise filter for clatter buffers.  Each nick
;; accumulates a count of signal messages (for example PRIVMSG) versus
;; noise messages (for example JOIN, PART, QUIT).  When a nick's
;; signal-to-noise ratio falls below `clatter-smart-threshold', its
;; noisy events are hidden via the buffer invisibility spec.  When a
;; nick produces its first signal, `clatter-smart-activate-hook' runs
;; so the UI can retroactively reveal the noise it had hidden.

;;; Code:

(defcustom clatter-smart-noise '(join part quit nick away)
  "Message types considered to be noise in clatter's smart filter."
  :type '(repeat (choice (const :tag "JOIN" join)
                         (const :tag "PART" part)
                         (const :tag "QUIT" quit)
                         (const :tag "NICK" nick)
                         (const :tag "MODE" mode)
                         (const :tag "AWAY" away)))
  :group 'clatter)

(defcustom clatter-smart-enabled t
  "When non-nil, hide smart-filtered noise in newly created buffers.

This adds the `noise' invisibility category to new clatter buffers when
`clatter-smart-noise' is non-nil.  Explicit `clatter-suppress-messages'
settings continue to be honored independently."
  :type 'boolean
  :group 'clatter)

(defcustom clatter-smart-threshold 0.5
  "SNR threshold under which inactive nicks' noisy message types are hidden."
  :type 'number
  :group 'clatter)

(defvar clatter-smart-data nil
  "Smart filter state keyed by nick.

Each value is a list (ACTIVE-P SIGNAL-COUNT NOISE-COUNT LAST-SIGNAL)
where LAST-SIGNAL is the float time of the nick's most recent
non-noise event.")

(defvar clatter-smart-activate-hook nil
  "Hook run when NICK becomes active (first ever signal) in BUF.
Called with (BUF NICK).  Lets the UI retroactively reveal the join (and
other presence) lines smart filtering had hidden for that nick.")

(defun clatter-smart-on (buf)
  "Get smart filter data for BUF."
  (with-current-buffer buf
    (or clatter-smart-data
        (setq-local clatter-smart-data
                    (make-hash-table :test 'equal)))))

(defun clatter-smart--entry-signal-count (entry)
  "Return signal count from smart state ENTRY."
  (or (nth 1 entry) 0))

(defun clatter-smart--entry-noise-count (entry)
  "Return noise count from smart state ENTRY."
  (or (nth 2 entry) 0))

(defun clatter-smart--entry-active-p (entry)
  "Return non-nil when smart state ENTRY has observed signal."
  (car entry))

(defun clatter-smart--entry-last-signal (entry)
  "Return float time of ENTRY's most recent signal, or 0."
  (or (nth 3 entry) 0))

(defun clatter-smart-put (buf nick elt &optional time)
  "Record ELT for NICK in BUF and return an SNR value if NICK is active.
TIME is the event's timestamp (any `float-time' input, default now);
it is stored as the nick's last signal for non-noise events."
  ;; (stringp elt) => t implies NICK changed to ELT.
  ;; Normalize nicks.
  (setq nick (downcase nick))
  (when (stringp elt)
    (setq elt (downcase elt)))
  (let* ((is-nick-change (and (stringp nick)
                              (stringp elt)))
         (data (clatter-smart-on buf))
         (entry (gethash nick data))
         (signal-count (clatter-smart--entry-signal-count entry))
         (noise-count (clatter-smart--entry-noise-count entry))
         (activep (clatter-smart--entry-active-p entry))
         (last-signal (clatter-smart--entry-last-signal entry))
         (is-noise (memq (if is-nick-change 'nick elt) clatter-smart-noise)))
    (when is-nick-change
      (remhash nick data)
      (setq nick elt))
    (setq signal-count (+ signal-count (if is-noise 0 1)))
    (setq noise-count (+ noise-count (if is-noise 1 0)))
    (setq activep (or activep (not is-noise)))
    (unless is-noise
      (setq last-signal (float-time (or time (current-time)))))
    (puthash nick (list activep signal-count noise-count last-signal) data)
    ;; First-ever signal: the nick was previously judged pure noise (its
    ;; join was hidden), and now becomes a participant.
    (when (and (not is-noise)
               (not (clatter-smart--entry-active-p entry)))
      (run-hook-with-args 'clatter-smart-activate-hook buf nick))
    (and activep
         ;; Float division: integer division would collapse the ratio to 0 or 1
         ;; and make `clatter-smart-threshold' meaningless.
         (if (zerop noise-count)
             most-positive-fixnum
           (/ (float signal-count) noise-count)))))

(defun clatter-smart-eval (buf nick elt)
  "Record ELT for NICK in BUF and return whether NICK is noisy."
  (let ((ratio-if-activep (clatter-smart-put buf nick elt)))
    (or (not ratio-if-activep)
        (< ratio-if-activep clatter-smart-threshold))))

(provide 'clatter-smart)

;;; clatter-smart.el ends here
