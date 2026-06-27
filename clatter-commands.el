;;; clatter-commands.el --- User commands for clatter.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; User slash-commands for clatter.el (/join, /part, /msg, /me, etc).
;; Ported from CLatter's core/commands.lisp.

;;; Code:

(require 'cl-lib)
(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-model)
(require 'clatter-ui)
(require 'clatter-list)
(require 'clatter-search)
(require 'clatter-pals)

;; --- Command dispatch ---

(defvar clatter-command-table (make-hash-table :test 'equal)
  "Hash table mapping command names to handler functions.")

(defun clatter-defcommand (name func &rest aliases)
  "Register FUNC as handler for command NAME.
ALIASES are alternative names for the same command."
  (puthash (downcase name) func clatter-command-table)
  (dolist (alias aliases)
    (puthash (downcase alias) func clatter-command-table)))

(defun clatter-execute-command (input)
  "Parse and execute INPUT as a /command.
INPUT is the full string including the leading /."
  (let* ((no-slash (substring input 1))
         (space-pos (cl-position ?\s no-slash))
         (cmd (downcase (if space-pos
                            (substring no-slash 0 space-pos)
                          no-slash)))
         (args (if space-pos
                   (string-trim-left (substring no-slash (1+ space-pos)))
                 ""))
         (handler (gethash cmd clatter-command-table)))
    (if handler
        (funcall handler args)
      (clatter-insert-error (current-buffer)
                            (format "Unknown command: /%s" cmd)))))

;; --- Helper to get current connection ---

(defun clatter--current-conn ()
  "Get the connection for the current buffer."
  (when clatter--network
    (clatter-get-connection clatter--network)))

(defun clatter--require-conn ()
  "Get current connection or signal an error message."
  (or (clatter--current-conn)
      (progn
        (clatter-insert-error (current-buffer) "Not connected")
        nil)))

;; --- Commands ---

(defun clatter-cmd-say (args)
  "Send ARGS to the current buffer as a message."
  (let ((trimmed (string-trim args)))
    (when (> (length trimmed) 0)
      (clatter--send-message trimmed))))

(defun clatter-cmd-join (args)
  "Join a channel; ARGS is \"CHANNEL [KEY]\"."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let* ((parts (split-string args))
             (channel (car parts))
             (key (cadr parts)))
        (if (and channel (> (length channel) 0))
            (clatter-send conn (clatter-irc-join channel key))
          (clatter-insert-error (current-buffer) "Usage: /join #channel [key]"))))))

(defun clatter-cmd-part (args)
  "Leave a channel; ARGS is \"[CHANNEL] [MESSAGE]\"."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let* ((parts (split-string args " " t))
             (channel (if (and (car parts)
                               (clatter-channel-name-p (car parts)))
                          (car parts)
                        clatter--target))
             (message (if (and (car parts)
                               (clatter-channel-name-p (car parts)))
                          (string-join (cdr parts) " ")
                        args)))
        (clatter-send conn (clatter-irc-part channel
                                             (unless (string-empty-p message)
                                               message)))))))

(defun clatter-cmd-msg (args)
  "Send a private message; ARGS is \"TARGET TEXT\"."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((space-pos (cl-position ?\s args)))
        (if space-pos
            (let ((target (substring args 0 space-pos))
                  (text (string-trim-left (substring args (1+ space-pos)))))
              (clatter-send conn (clatter-irc-privmsg target text))
              ;; Open query buffer and show the message (skip if echo-message handles it)
              (let ((buf (clatter-get-or-create-buffer clatter--network target)))
                (clatter-ui-setup-buffer-if-needed buf)
                (unless (member "echo-message" (clatter-connection-cap-enabled conn))
                  (clatter-insert-privmsg buf (clatter-connection-nick conn) text conn))))
          (clatter-insert-error (current-buffer) "Usage: /msg target message"))))))

(defun clatter-cmd-me (args)
  "Send a CTCP ACTION using ARGS as the action text."
  (let ((conn (clatter--require-conn)))
    (when conn
      (when (and clatter--target (not (string= clatter--target "*server*")))
        (let ((ctcp-msg (format "\C-aACTION %s\C-a" args)))
          (clatter-send conn (clatter-irc-privmsg clatter--target ctcp-msg))
          (unless (member "echo-message" (clatter-connection-cap-enabled conn))
            (clatter-insert-action (current-buffer)
                                   (clatter-connection-nick conn)
                                   args conn)))))))

(defun clatter-cmd-nick (args)
  "Change nick to the NEWNICK given in ARGS."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((new-nick (car (split-string args))))
        (if (and new-nick (> (length new-nick) 0))
            (clatter-send conn (clatter-irc-nick new-nick))
          (clatter-insert-error (current-buffer) "Usage: /nick newnick"))))))

(defun clatter-cmd-topic (args)
  "View or set the channel topic; ARGS is the new topic."
  (let ((conn (clatter--require-conn)))
    (when conn
      (when clatter--target
        (if (> (length args) 0)
            (clatter-send conn (clatter-irc-topic clatter--target args))
          (clatter-send conn (clatter-irc-topic clatter--target)))))))

(defun clatter-cmd-kick (args)
  "Kick a user; ARGS is \"NICK [REASON]\"."
  (let ((conn (clatter--require-conn)))
    (when conn
      (when clatter--target
        (let* ((parts (split-string args " " t))
               (nick (car parts))
               (reason (string-join (cdr parts) " ")))
          (if nick
              (clatter-send conn (clatter-irc-kick clatter--target nick
                                                   (unless (string-empty-p reason)
                                                     reason)))
            (clatter-insert-error (current-buffer) "Usage: /kick nick [reason]")))))))

(defun clatter-cmd-mode (args)
  "Handle /mode [MODE] [ARGS]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((target (or clatter--target "")))
        (if (> (length args) 0)
            (clatter-send conn (format "MODE %s %s" target args))
          (clatter-send conn (clatter-irc-mode target)))))))

(defun clatter-cmd-whois (args)
  "Request WHOIS for the NICK in ARGS."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((nick (car (split-string args))))
        (if (and nick (> (length nick) 0))
            (clatter-send conn (clatter-irc-whois nick))
          (clatter-insert-error (current-buffer) "Usage: /whois nick"))))))

(defun clatter-cmd-away (args)
  "Set away using ARGS as the message.  Empty ARGS clears away."
  (let ((conn (clatter--require-conn)))
    (when conn
      (clatter-send conn (clatter-irc-away
                          (unless (string-empty-p args) args))))))

(defun clatter-cmd-quit (args)
  "Disconnect, using ARGS as the optional quit message."
  (let ((network clatter--network))
    (clatter-disconnect network (if (string-empty-p args) nil args))))

(defun clatter-cmd-raw (args)
  "Send ARGS to the server as a raw IRC line."
  (let ((conn (clatter--require-conn)))
    (when conn
      (if (> (length args) 0)
          (clatter-send conn args)
        (clatter-insert-error (current-buffer) "Usage: /raw IRC-COMMAND")))))

(defun clatter-cmd-query (args)
  "Open a query buffer for the NICK given in ARGS."
  (let* ((nick (car (split-string args)))
         (network clatter--network))
    (if (and nick (> (length nick) 0))
        (let ((buf (clatter-get-or-create-buffer network nick 'query)))
          (clatter-ui-setup-buffer-if-needed buf)
          (switch-to-buffer buf))
      (clatter-insert-error (current-buffer) "Usage: /query nick"))))

(defun clatter-cmd-names (args)
  "Request NAMES; ARGS is an optional channel."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((channel (if (> (length args) 0)
                         (car (split-string args))
                       clatter--target)))
        (when channel
          (clatter-send conn (clatter-irc-names channel)))))))

(defun clatter-cmd-invite (args)
  "Invite a user; ARGS is \"NICK [CHANNEL]\"."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let* ((parts (split-string args))
             (nick (car parts))
             (channel (or (cadr parts) clatter--target)))
        (if nick
            (clatter-send conn (clatter-irc-invite nick channel))
          (clatter-insert-error (current-buffer)
                                "Usage: /invite nick [channel]"))))))

(defun clatter-cmd-ctcp (args)
  "Handle /ctcp TARGET COMMAND [ARGS]."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let* ((parts (split-string args " " t))
             (target (nth 0 parts))
             (command (upcase (or (nth 1 parts) "")))
             (rest (string-join (nthcdr 2 parts) " ")))
        (if (and target (> (length command) 0))
            (clatter-send conn (clatter-irc-ctcp-request target command
                                                         (unless (string-empty-p rest)
                                                           rest)))
          (clatter-insert-error (current-buffer)
                                "Usage: /ctcp target command [args]"))))))

(defun clatter-cmd-monitor (args)
  "Control MONITOR; ARGS is \"+nick,-nick,C,L,S\"."
  (let ((conn (clatter--require-conn)))
    (when conn
      (cond
       ((string-prefix-p "+" args)
        (clatter-send conn (clatter-irc-monitor-add (substring args 1))))
       ((string-prefix-p "-" args)
        (clatter-send conn (clatter-irc-monitor-remove (substring args 1))))
       ((string-equal (upcase args) "C")
        (clatter-send conn (clatter-irc-monitor-clear)))
       ((string-equal (upcase args) "L")
        (clatter-send conn (clatter-irc-monitor-list)))
       ((string-equal (upcase args) "S")
        (clatter-send conn (clatter-irc-monitor-status)))
       (t
        (clatter-insert-error (current-buffer)
                              "Usage: /monitor +nick / -nick / C / L / S"))))))

(defun clatter-cmd-clear (_args)
  "Handle /clear - clear current buffer."
  (let ((inhibit-read-only t))
    (when clatter--prompt-marker
      (delete-region (point-min) clatter--prompt-marker))))

(defun clatter-cmd-buffers (_args)
  "Handle /buffers - list all buffers."
  (let ((lines nil))
    (dolist (buf (clatter-all-buffers))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (push (format "  %s (%s)" (buffer-name) clatter--buffer-type) lines))))
    (clatter-insert-system (current-buffer)
                           (format "Buffers:\n%s"
                                   (string-join (nreverse lines) "\n")))))

;; --- NickServ shortcuts ---

(defun clatter-cmd-ns (args)
  "Send ARGS to NickServ (shortcut for /msg NickServ)."
  (let ((conn (clatter--require-conn)))
    (when conn
      (clatter-send conn (clatter-irc-privmsg "NickServ" args)))))

(defun clatter-cmd-cs (args)
  "Send ARGS to ChanServ (shortcut for /msg ChanServ)."
  (let ((conn (clatter--require-conn)))
    (when conn
      (clatter-send conn (clatter-irc-privmsg "ChanServ" args)))))

(defun clatter--nickserv-recover (conn verb nick)
  "Send NickServ VERB (\"GHOST\" or \"REGAIN\") for NICK on CONN.
Includes the network password if one is configured/available."
  (let* ((network (clatter-connection-network-id conn))
         (password (clatter-get-password network))
         (cmd (if password
                  (format "%s %s %s" verb nick password)
                (format "%s %s" verb nick))))
    (clatter-send conn (clatter-irc-privmsg "NickServ" cmd))
    (clatter-insert-system (current-buffer)
                           (format "Sent NickServ %s for %s" verb nick))))

(defun clatter-cmd-ghost (args)
  "Disconnect a ghost session holding a nick; ARGS is an optional NICK.
With no NICK, uses your configured (desired) nick.  GHOST kills the
other session but does NOT rename you; the reclaim timer (or a manual
/nick) then takes the freed nick."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((nick (or (car (split-string (string-trim args)))
                      (clatter-connection-desired-nick conn)
                      (clatter-connection-nick conn))))
        (clatter--nickserv-recover conn "GHOST" nick)))))

(defun clatter-cmd-regain (args)
  "Reclaim a nick, renaming you to it; ARGS is an optional NICK.
With no NICK, uses your configured (desired) nick.  REGAIN both frees
the nick from any other session and changes your nick to it.  Note: if
another client authed to your account is online, it can REGAIN back,
causing a kill loop - use /ghost and wait instead in that case."
  (let ((conn (clatter--require-conn)))
    (when conn
      (let ((nick (or (car (split-string (string-trim args)))
                      (clatter-connection-desired-nick conn)
                      (clatter-connection-nick conn))))
        (clatter--nickserv-recover conn "REGAIN" nick)))))

(defun clatter-cmd-reclaim (args)
  "Control automatic nick reclaim; ARGS is \"[on|off]\".
With no argument, report the current state.  Turning it off also
cancels any reclaim timer that is currently running."
  (let ((arg (downcase (string-trim args))))
    (cond
     ((member arg '("off" "no" "disable" "stop"))
      (setq clatter-nick-reclaim-enabled nil)
      (let ((conn (clatter--current-conn)))
        (when (and conn (clatter-connection-nick-reclaim-timer conn))
          (cancel-timer (clatter-connection-nick-reclaim-timer conn))
          (setf (clatter-connection-nick-reclaim-timer conn) nil)))
      (clatter-insert-system (current-buffer) "Nick reclaim disabled"))
     ((member arg '("on" "yes" "enable" "start"))
      (setq clatter-nick-reclaim-enabled t)
      (clatter-insert-system (current-buffer) "Nick reclaim enabled"))
     (t
      (clatter-insert-system
       (current-buffer)
       (format "Nick reclaim is %s"
               (if clatter-nick-reclaim-enabled "enabled" "disabled")))))))

(defun clatter-cmd-suppress (args)
  "Suppress the message types listed in ARGS (\"[TYPE...]\").
With no args, show current suppressions.
With `all', suppress join/part/quit/nick/mode/away.
With `none', clear all suppressions."
  (let ((types (split-string (string-trim args) " " t)))
    (cond
     ((null types)
      (message "Suppressed: %s"
               (if (remove 'clatter-fool buffer-invisibility-spec)
                   (mapconcat #'symbol-name
                              (remove 'clatter-fool buffer-invisibility-spec)
                              " ")
                 "none")))
     ((string-equal (car types) "all")
      (setq buffer-invisibility-spec (list 'join 'part 'quit 'nick 'mode 'away 'muted))
      (clatter--apply-current-buffer-fools-visibility))
     ((string-equal (car types) "none")
      (setq buffer-invisibility-spec nil)
      (clatter--apply-current-buffer-fools-visibility))
     (t
      (dolist (type types)
        (let ((sym (intern type)))
          (unless (memq sym buffer-invisibility-spec)
            (push sym buffer-invisibility-spec))))
      (clatter--apply-current-buffer-fools-visibility)
      (message "Suppressed: %s"
               (mapconcat #'symbol-name
                          (remove 'clatter-fool buffer-invisibility-spec)
                          " "))))))

(defun clatter-cmd-unsuppress (args)
  "Stop suppressing the message types listed in ARGS (\"TYPE...\")."
  (let ((types (split-string (string-trim args) " " t)))
    (dolist (type types)
      (setq buffer-invisibility-spec
            (delq (intern type) buffer-invisibility-spec)))
    (clatter--apply-current-buffer-fools-visibility)
    (message "Suppressed: %s"
             (if (remove 'clatter-fool buffer-invisibility-spec)
                 (mapconcat #'symbol-name
                            (remove 'clatter-fool buffer-invisibility-spec)
                            " ")
               "none"))))

;; --- Register all commands ---

(clatter-defcommand "say" #'clatter-cmd-say)
(clatter-defcommand "join" #'clatter-cmd-join "j")
(clatter-defcommand "part" #'clatter-cmd-part "leave")
(clatter-defcommand "msg" #'clatter-cmd-msg)
(clatter-defcommand "me" #'clatter-cmd-me)
(clatter-defcommand "nick" #'clatter-cmd-nick)
(clatter-defcommand "topic" #'clatter-cmd-topic)
(clatter-defcommand "kick" #'clatter-cmd-kick)
(clatter-defcommand "mode" #'clatter-cmd-mode)
(clatter-defcommand "whois" #'clatter-cmd-whois)
(clatter-defcommand "away" #'clatter-cmd-away)
(clatter-defcommand "quit" #'clatter-cmd-quit "q")
(clatter-defcommand "raw" #'clatter-cmd-raw)
(clatter-defcommand "query" #'clatter-cmd-query)
(clatter-defcommand "names" #'clatter-cmd-names "members")
(clatter-defcommand "invite" #'clatter-cmd-invite)
(clatter-defcommand "ctcp" #'clatter-cmd-ctcp)
(clatter-defcommand "monitor" #'clatter-cmd-monitor)
(clatter-defcommand "clear" #'clatter-cmd-clear)
(clatter-defcommand "buffers" #'clatter-cmd-buffers)
(clatter-defcommand "suppress" #'clatter-cmd-suppress)
(clatter-defcommand "unsuppress" #'clatter-cmd-unsuppress)
(clatter-defcommand "ns" #'clatter-cmd-ns)
(clatter-defcommand "cs" #'clatter-cmd-cs)
(clatter-defcommand "ghost" #'clatter-cmd-ghost)
(clatter-defcommand "regain" #'clatter-cmd-regain)
(clatter-defcommand "reclaim" #'clatter-cmd-reclaim)

;; --- Ignore commands ---

(defun clatter-cmd-ignore (args)
  "Ignore a nick or pattern given in ARGS.
Usage: /ignore NICK-OR-PATTERN.
Supports glob wildcards (* and ?).
With no argument, show the current ignore list."
  (let ((pattern (string-trim args)))
    (if (string-empty-p pattern)
        (clatter-insert-system
         (current-buffer)
         (if clatter-ignore-list
             (format "Ignore list: %s" (string-join clatter-ignore-list ", "))
           "Ignore list is empty"))
      (unless (or (seq-contains-p pattern ?\*)
                  (seq-contains-p pattern ?\?)
                  (seq-contains-p pattern ?\[))
        (setq pattern (format "%s!*@*" pattern)))
      (if (member (downcase pattern) (mapcar #'downcase clatter-ignore-list))
          (clatter-insert-system (current-buffer)
                                  (format "%s is already ignored" pattern))
        (push pattern clatter-ignore-list)
        (clatter-insert-system (current-buffer)
                                (format "Now ignoring %s" pattern))))))

(defun clatter-cmd-unignore (args)
  "Remove a nick or pattern (ARGS) from the ignore list.
Usage: /unignore NICK-OR-PATTERN."
  (let ((pattern (string-trim args)))
    (if (string-empty-p pattern)
        (clatter-insert-error (current-buffer) "Usage: /unignore NICK-OR-PATTERN")
      (unless (or (seq-contains-p pattern ?\*)
                  (seq-contains-p pattern ?\?)
                  (seq-contains-p pattern ?\[))
        (setq pattern (format "%s!*@*" pattern)))
      (let ((before (length clatter-ignore-list)))
        (setq clatter-ignore-list
              (cl-remove pattern clatter-ignore-list
                         :test #'string-equal-ignore-case))
        (if (< (length clatter-ignore-list) before)
            (clatter-insert-system (current-buffer)
                                    (format "No longer ignoring %s" pattern))
          (clatter-insert-system (current-buffer)
                                  (format "%s was not in the ignore list" pattern)))))))

;; --- Pals and fools commands ---

(defun clatter-cmd-pal (args)
  "Add nick ARGS to the pals list; with no argument, show the list.
Pals' nicks are highlighted with the `clatter-pal' face."
  (let ((nick (string-trim args)))
    (cond
     ((string-empty-p nick)
      (clatter-insert-system
       (current-buffer)
       (if clatter-pals
           (format "Pals: %s" (string-join clatter-pals ", "))
         "Pals list is empty")))
     ((clatter-pal-p nick)
      (clatter-insert-system (current-buffer)
                             (format "%s is already a pal" nick)))
     (t
      (setq clatter-pals (clatter--nick-list-add nick clatter-pals))
      (clatter-insert-system (current-buffer)
                             (format "%s is now a pal" nick))))))

(defun clatter-cmd-unpal (args)
  "Remove nick ARGS from the pals list."
  (let ((nick (string-trim args)))
    (if (string-empty-p nick)
        (clatter-insert-error (current-buffer) "Usage: /unpal NICK")
      (if (clatter-pal-p nick)
          (progn
            (setq clatter-pals (clatter--nick-list-remove nick clatter-pals))
            (clatter-insert-system (current-buffer)
                                   (format "%s is no longer a pal" nick)))
        (clatter-insert-system (current-buffer)
                               (format "%s was not a pal" nick))))))

(defun clatter-cmd-pals (_args)
  "Show the pals list."
  (clatter-insert-system
   (current-buffer)
   (if clatter-pals
       (format "Pals: %s" (string-join clatter-pals ", "))
     "Pals list is empty")))

(defun clatter-cmd-fool (args)
  "Add nick ARGS to the fools list; with no argument, show the list.
Messages from a fool are muted (hidden)."
  (let ((nick (string-trim args)))
    (cond
     ((string-empty-p nick)
      (clatter-insert-system
       (current-buffer)
       (if clatter-fools
           (format "Fools: %s" (string-join clatter-fools ", "))
         "Fools list is empty")))
     ((clatter-fool-p nick)
      (clatter-insert-system (current-buffer)
                             (format "%s is already a fool" nick)))
     (t
      (setq clatter-fools (clatter--nick-list-add nick clatter-fools))
      (clatter-insert-system (current-buffer)
                             (format "Now muting %s" nick))))))

(defun clatter-cmd-unfool (args)
  "Remove nick ARGS from the fools list."
  (let ((nick (string-trim args)))
    (if (string-empty-p nick)
        (clatter-insert-error (current-buffer) "Usage: /unfool NICK")
      (if (clatter-fool-p nick)
          (progn
            (setq clatter-fools (clatter--nick-list-remove nick clatter-fools))
            (clatter-insert-system (current-buffer)
                                   (format "No longer muting %s" nick)))
        (clatter-insert-system (current-buffer)
                               (format "%s was not a fool" nick))))))

(defun clatter-cmd-fools (_args)
  "Show the fools list."
  (clatter-insert-system
   (current-buffer)
   (if clatter-fools
       (format "Fools: %s" (string-join clatter-fools ", "))
     "Fools list is empty")))

(defun clatter--apply-current-buffer-fools-visibility ()
  "Apply `clatter-fools-visible' to the current buffer."
  (if clatter-fools-visible
      (remove-from-invisibility-spec 'clatter-fool)
    (add-to-invisibility-spec 'clatter-fool)))

(defun clatter--apply-fools-visibility ()
  "Apply `clatter-fools-visible' to every live clatter buffer."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'clatter-mode)
        (clatter--apply-current-buffer-fools-visibility)
        (force-window-update buf)))))

(defun clatter-toggle-fools (&optional arg)
  "Toggle visibility of messages from `clatter-fools'.
With prefix ARG, show fools when ARG is positive and hide them otherwise."
  (interactive "P")
  (setq clatter-fools-visible
        (if arg
            (> (prefix-numeric-value arg) 0)
          (not clatter-fools-visible)))
  (clatter--apply-fools-visibility)
  (message "Fools are now %s"
           (if clatter-fools-visible "shown" "hidden")))

(defun clatter-cmd-toggle-fools (_args)
  "Toggle whether fool messages are shown or hidden."
  (clatter-toggle-fools))

(defun clatter-cmd-close (_args)
  "Close (kill) the current clatter buffer.
For channels, sends PART first."
  (let ((network clatter--network)
        (target clatter--target)
        (buf-type clatter--buffer-type)
        (buf (current-buffer)))
    (when (and network target)
      ;; PART from channel if needed
      (when (eq buf-type 'channel)
        (let ((conn (clatter-get-connection network)))
          (when (and conn (clatter-connection-process conn)
                     (process-live-p (clatter-connection-process conn)))
            (clatter-send conn (clatter-irc-part target)))))
      ;; Remove from registry and kill buffer
      (clatter-remove-buffer network target)
      (kill-buffer buf))))

(clatter-defcommand "close" #'clatter-cmd-close "wc")
(clatter-defcommand "ignore" #'clatter-cmd-ignore)
(clatter-defcommand "unignore" #'clatter-cmd-unignore)
(clatter-defcommand "pal" #'clatter-cmd-pal)
(clatter-defcommand "unpal" #'clatter-cmd-unpal)
(clatter-defcommand "pals" #'clatter-cmd-pals)
(clatter-defcommand "fool" #'clatter-cmd-fool)
(clatter-defcommand "unfool" #'clatter-cmd-unfool)
(clatter-defcommand "fools" #'clatter-cmd-fools)
(clatter-defcommand "toggle-fools" #'clatter-cmd-toggle-fools "fools-toggle")

(defun clatter-cmd-list (args)
  "Open the interactive channel list browser, filtered by ARGS."
  (let ((conn (clatter--current-conn)))
    (if conn
        (clatter-list-request conn (string-trim args))
      (message "Not connected."))))

(clatter-defcommand "list" #'clatter-cmd-list)

(defun clatter-cmd-search (args)
  "Search all IRC logs for ARGS.
Usage: /search QUERY."
  (let ((query (string-trim args)))
    (if (string-empty-p query)
        (call-interactively #'clatter-search)
      (clatter-search query))))

(defun clatter-cmd-searchhere (args)
  "Search the current channel logs for ARGS.
Usage: /searchhere QUERY."
  (let ((query (string-trim args)))
    (if (string-empty-p query)
        (call-interactively #'clatter-search-channel)
      (clatter-search-channel query))))

(clatter-defcommand "search" #'clatter-cmd-search "grep" "find")
(clatter-defcommand "searchhere" #'clatter-cmd-searchhere)

(defun clatter-cmd-reply (args)
  "Reply to the selected message with ARGS as the text.
Usage: /reply TEXT.
Uses +draft/reply tag to thread the response."
  (let* ((text (string-trim args))
         (msgid (and mouse-secondary-overlay
                     (eq (current-buffer) (overlay-buffer mouse-secondary-overlay))
                     (get-text-property (overlay-start mouse-secondary-overlay) 'clatter-msgid)))
         (target clatter--target)
         (conn (clatter--current-conn)))
    (cond
     ((string-empty-p text)
      (message "Usage: /reply <message>"))
     ((null msgid)
      (message "No message to reply to (select a message using the secondary selection i.e. M-<mouse-1>)"))
     ((null conn)
      (message "Not connected"))
     (t
      (clatter-send conn (format "@+draft/reply=%s PRIVMSG %s :%s"
                                 msgid target text))
      ; Clear secondary selection
      (save-mark-and-excursion (secondary-selection-to-region))
      ;; Echo if echo-message not enabled
      (unless (member "echo-message" (clatter-connection-cap-enabled conn))
        (clatter-insert-privmsg (current-buffer)
                                (clatter-connection-nick conn)
                                text conn))))))

(clatter-defcommand "reply" #'clatter-cmd-reply "r")

(defun clatter-cmd-react (args)
  "React to the selected message with the emoji in ARGS.
Usage: /react EMOJI.
Uses +draft/react tag via TAGMSG."
  (let* ((emoji (string-trim args))
         (msgid (and mouse-secondary-overlay
                     (eq (current-buffer) (overlay-buffer mouse-secondary-overlay))
                     (get-text-property (overlay-start mouse-secondary-overlay) 'clatter-msgid)))
         (target clatter--target)
         (conn (clatter--current-conn)))
    (cond
     ((string-empty-p emoji)
      (message "Usage: /react <emoji>"))
     ((null msgid)
      (message "No message to react to (select a message using the secondary selection i.e. M-<mouse-1>)"))
     ((null conn)
      (message "Not connected"))
     (t
      (clatter-send conn (format "@+draft/react=%s;+draft/reply=%s TAGMSG %s"
                                 emoji msgid target))
      ; Clear secondary selection
      (save-mark-and-excursion (secondary-selection-to-region))))))

(clatter-defcommand "react" #'clatter-cmd-react)

;; --- DCC ---

(require 'clatter-dcc)
(clatter-dcc-setup)
(clatter-defcommand "dcc" #'clatter-cmd-dcc)

;; --- Autojoin persistence ---

(defun clatter--update-autojoin (network-id channel action)
  "Update :autojoin for NETWORK-ID via the customize system.
CHANNEL is the channel name, ACTION is \\='add or \\='remove.
Persists the change to `custom-file' (or init file)."
  (let* ((net-config (assoc network-id clatter-networks))
         (config (cdr net-config))
         (current (plist-get config :autojoin)))
    (unless net-config
      (error "Network \"%s\" not found in clatter-networks" network-id))
    (let ((new-autojoin
           (pcase action
             ('add (if (member channel current)
                       (progn (message "[clatter] %s already in autojoin for %s"
                                      channel network-id)
                              nil)
                     (append current (list channel))))
             ('remove (if (member channel current)
                          (remove channel current)
                        (progn (message "[clatter] %s not in autojoin for %s"
                                       channel network-id)
                               nil))))))
      (when new-autojoin
        ;; Update the live config
        (plist-put (cdr net-config) :autojoin new-autojoin)
        ;; Persist via customize
        (customize-save-variable 'clatter-networks clatter-networks)
        (message "[clatter] Autojoin %s: %s on %s"
                 (if (eq action 'add) "added" "removed")
                 channel network-id)))))

(defun clatter-cmd-autojoin (args)
  "Handle /autojoin commands given in ARGS.
Usage:
  /autojoin add #channel    - add channel to autojoin and persist
  /autojoin remove #channel - remove channel from autojoin and persist
  /autojoin list            - show current autojoin channels"
  (let* ((parts (split-string (string-trim args)))
         (subcmd (downcase (or (car parts) "list")))
         (channel (cadr parts))
         (conn (clatter--current-conn))
         (network-id (when conn (clatter-connection-network-id conn))))
    (pcase subcmd
      ("add"
       (let ((ch (or channel clatter--target)))
         (if (and ch network-id)
             (clatter--update-autojoin network-id ch 'add)
           (message "[clatter] Usage: /autojoin add #channel"))))
      ("remove"
       (let ((ch (or channel clatter--target)))
         (if (and ch network-id)
             (clatter--update-autojoin network-id ch 'remove)
           (message "[clatter] Usage: /autojoin remove #channel"))))
      ("list"
       (if network-id
           (let* ((config (cdr (assoc network-id clatter-networks)))
                  (channels (plist-get config :autojoin)))
             (message "[clatter] Autojoin for %s: %s"
                      network-id
                      (if channels
                          (string-join channels " ")
                        "(none)")))
         (message "[clatter] Not connected")))
      (_
       (message "[clatter] Usage: /autojoin add|remove|list [#channel]")))))

(clatter-defcommand "autojoin" #'clatter-cmd-autojoin)

(provide 'clatter-commands)

;;; clatter-commands.el ends here
