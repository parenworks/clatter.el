;;; clatter.el --- An IRCv3-compliant IRC client -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Glenn Thompson
;; Author: Glenn Thompson <glenn@paren.works>
;; SPDX-License-Identifier: MIT
;; URL: https://github.com/parenworks/clatter.el
;; Version: 0.8.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: comm, irc

;;; Commentary:

;; clatter.el is a dedicated, IRCv3-compliant IRC client for Emacs.
;; Pure Elisp, requiring only Emacs 30.1+ and curl (for async URL/image fetching).
;;
;; Spiritual successor to CLatter (Common Lisp TUI IRC client),
;; redesigned from scratch for Emacs.
;;
;; Features:
;; - Full IRC protocol (RFC 1459/2812)
;; - IRCv3 capability negotiation (CAP LS 302)
;; - SASL authentication (PLAIN and EXTERNAL)
;; - Message tags, batch, labeled-response, chathistory
;; - MONITOR, typing indicators, CTCP
;; - TLS/SSL with client certificate support
;; - Buffer-per-channel with nick colorization
;; - auth-source integration for passwords
;; - Reconnection with exponential backoff
;;
;; Quick start:
;;
;;   (require 'clatter)
;;   (require 'gnutls)
;;   (setopt clatter-networks
;;     '(("libera"
;;        :server "irc.libera.chat"
;;        :port 6697
;;        :tls t
;;        :nick "yournick"
;;        :sasl plain
;;        :autojoin ("#systemcrafters" "#commonlisp"))))
;;   (clatter-setup)
;;   (clatter-connect "libera")

;;; Code:

(require 'clatter-config)
(require 'clatter-protocol)
(require 'clatter-connection)
(require 'clatter-socks)
(require 'clatter-cap)
(require 'clatter-handlers)
(require 'clatter-model)
(require 'clatter-ui)
(require 'clatter-commands)
(require 'clatter-actions)
(require 'clatter-track)
(require 'clatter-notify)
(require 'clatter-completion)
(require 'clatter-rawlog)
(require 'clatter-chathistory)
(require 'clatter-read-marker)
(require 'clatter-nicklist)
(require 'clatter-log)
(require 'clatter-url-preview)
(require 'clatter-feed)
(require 'clatter-evil)
(require 'clatter-soju)
(require 'clatter-stats)

;; --- Autoload entry points ---

;;;###autoload
(defun clatter (network)
  "Connect to IRC NETWORK.
NETWORK should be the name of a network defined in `clatter-networks'.
Interactively, prompts with completion."
  (interactive
   (list (completing-read "Connect to network: "
                          (mapcar #'car clatter-networks)
                          nil t)))
  (clatter-connect network))

;;;###autoload
(defun clatter-quick-connect (server nick &optional port tls)
  "Quickly connect to SERVER as NICK.
PORT defaults to 6697, TLS defaults to t.
Creates a transient network entry."
  (interactive
   (list (read-string "Server: ")
         (read-string "Nick: " (user-login-name))
         (read-number "Port: " 6697)
         (y-or-n-p "Use TLS? ")))
  (let ((network-id (format "quick-%s" server)))
    (clatter-connect network-id
                     :server server
                     :nick nick
                     :port (or port 6697)
                     :tls (if (null tls) t tls))))

;; `clatter-disconnect' is defined in clatter-connection.el which properly
;; disables auto-reconnect before killing the process.

;; --- Status ---

(defun clatter-status ()
  "Display status of all connections."
  (interactive)
  (let ((lines nil))
    (maphash (lambda (id conn)
               (push (format "%-20s %-15s nick: %s  caps: %s"
                             id
                             (clatter-connection-state conn)
                             (or (clatter-connection-nick conn) "-")
                             (length (clatter-connection-cap-enabled conn)))
                     lines))
             clatter-connections)
    (if lines
        (message "clatter connections:\n%s" (string-join (nreverse lines) "\n"))
      (message "No clatter connections"))))

(defun clatter--on-disconnect (network _event)
  "Remove dead buffers for NETWORK from the buffer list.
Added to `clatter-disconnect-hook' by `clatter-setup'.  The disconnect
EVENT string is ignored."
  (let ((buf (clatter-get-server-buffer network)))
    (when (and buf (not (buffer-live-p buf)))
      (clatter-remove-buffer network "*server*"))))

;; --- Setup ---

;;;###autoload
(defun clatter-setup ()
  "Enable clatter's global hooks and optional features.

Merely loading clatter has no side effects: it installs no global hooks,
enables no optional features, and prints nothing.  Call this once from
your configuration to wire everything up:

  (require \\='clatter)
  (clatter-setup)

This installs the disconnect and Emacs-exit cleanup handlers and enables
the optional extras that are turned on through their own user options:
activity tracking, desktop notifications, chathistory, read markers,
per-buffer logging, URL previews and the feed inbox buffer.  To leave
a feature off, customize the corresponding option (for example
`clatter-track-enabled' or `clatter-log-enable') to nil before calling
this function.

Calling it more than once is harmless."
  (interactive)
  ;; Cleanup handlers.
  (add-hook 'clatter-disconnect-hook #'clatter--on-disconnect)
  (add-hook 'kill-emacs-hook #'clatter--quit-on-exit)
  ;; Suspend/resume detection: re-establish connections promptly after the
  ;; machine wakes, instead of waiting up to `clatter-ping-timeout' seconds
  ;; for the health check to notice a half-open link.
  (when clatter-suspend-detect (clatter--start-resume-detector))
  ;; Optional features, each gated by its own user option.
  (when clatter-track-enabled (clatter-track-enable))
  (when clatter-notify-enabled (clatter-notify-enable))
  (when clatter-chathistory-enabled (clatter-chathistory-enable))
  (when clatter-read-marker-enabled (clatter-read-marker-enable))
  (when clatter-log-enable (clatter-log-init))
  (when clatter-url-preview-enable (clatter-url-preview-init))
  (when clatter-soju-enabled (clatter-soju-enable))
  (when clatter-feed-enabled (clatter-feed-enable)))

(provide 'clatter)

;;; clatter.el ends here
