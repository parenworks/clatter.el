;;; clatter-stats.el --- Message-count leaderboard per channel -*- lexical-binding: t; -*-

;; Count PRIVMSG/ACTION messages per nick in the current clatter channel
;; buffer using the clatter-sender / clatter-msg-type text properties.

;;; Code:
(require 'cl-lib)
(require 'clatter-model)

(defun clatter-stats-leaderboard ()
  "Show a leaderboard of message counts per nick in the current channel.
Scans the buffer via the clatter-sender and clatter-msg-type text
properties; only what is in the buffer (including loaded backlog) is
counted."
  (interactive)
  (unless (bound-and-true-p clatter--target)
    (user-error "Not in a clatter channel buffer"))
  (let ((counts (make-hash-table :test 'equal))   ; dcase-nick -> count
        (nicks (make-hash-table :test 'equal))    ; dcase-nick -> display nick
        (last-seen (make-hash-table :test 'equal))
        (oldest nil)
        (newest nil)
        (total 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let* ((sender (get-text-property (point) 'clatter-sender))
               (type (get-text-property (point) 'clatter-msg-type))
               (time (get-text-property (point) 'clatter-server-time))
               (key (and sender (downcase sender))))
          (when (and key (memq type '(privmsg action)))
            (cl-incf total)
            (cl-incf (gethash key counts 0))
            (puthash key sender nicks)
            (when time
              (puthash key time last-seen)
              (when (or (null oldest) (time-less-p time oldest))
                (setq oldest time))
              (when (or (null newest) (time-less-p newest time))
                (setq newest time)))))
        (forward-line 1)))
    (if (zerop total)
        (message "No messages in this buffer")
      (let* ((target clatter--target)
             (span (when (and oldest newest)
                     (/ (truncate (- (float-time newest)
                                     (float-time oldest)))
                        3600)))
             (rows (sort (hash-table-keys counts)
                         (lambda (a b)
                           (or (> (gethash a counts)
                                  (gethash b counts))
                               (and (= (gethash a counts)
                                       (gethash b counts))
                                    (string< a b))))))
             (buf (get-buffer-create "*clatter-leaderboard*"))
             (inhibit-read-only t))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "%s — %d messages from %d nicks%s\n\n"
                         target total (length rows)
                         (if span (format " over ~%dh" span) "")))
          (dolist (key rows)
            (insert (format "%5d  %-20s %s\n"
                            (gethash key counts)
                            (gethash key nicks)
                            (let ((ts (gethash key last-seen)))
                              (if ts
                                  (format-time-string "%Y-%m-%d %H:%M" ts)
                                "")))))
          (goto-char (point-min))
          (special-mode))
        (pop-to-buffer buf)))))

(provide 'clatter-stats)
