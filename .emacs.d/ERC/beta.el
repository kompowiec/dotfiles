;; Load ERC on startup
(add-hook 'erc-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (erc-timestamp-mode t)
            (erc-spelling-mode t)))

;; Rename server buffers to reflect the current network name instead
;; of SERVER:PORT (e.g., "Libera.Chat" instead of
;; "irc.libera.chat:6667").  This is useful when using a bouncer like
;; ZNC where you have multiple connections to the same server.
(setq erc-rename-buffers t)

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
;;(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
;;(setq erc-kill-server-buffer-on-quit t)

;This section sets up erc to highlight specific keywords and notify when mentioned in any channel.
(require 'erc-match)
(setq erc-keywords '("komp"))
(erc-match-mode)

;This section sets up erc to track and highlight activity in specified channels.
(require 'erc-track)
(erc-track-mode t) ; was (erc-track-modified-channels-mode t)
                   ; Note: erc-track-modified-channels-mode changed
                   ; to erc-track-mode as of erc-track.el
                   ; CVS revision 1.23 (November 2002)
;; Set ERC notifications
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
(setq erc-track-showcount t)
(setq erc-track-shorten-start 5)

(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))

;This section configures erc to fill lines that go beyond a specified column limit.
;(require 'erc-fill)
(erc-fill-mode t)

(require 'erc-ring)
(erc-ring-mode t)

;This section sets up erc to track and notify when a channel is affected by a netsplit.
(require 'erc-netsplit)
(erc-netsplit-mode t)


;This section disables erc button mode, which can cause erc to become slow when the buffer has many links.
(erc-button-mode nil) ;slow

;; logging:
;This section configures erc to save IRC logs to a specific directory and set options for the logging behavior.
(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/.irclogs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)
(setq erc-log-file-coding-system 'utf-8)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;This section sets up a buffer kill-emacs advice that saves erc logs that were not yet saved.
(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                             (not (null buffer-file-name)))))))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))
;; end logging

;; Truncate buffers so they don't hog core.
;This section sets up erc to truncate the buffer when it reaches a maximum size to avoid taking up too much memory.
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)


;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
;This section defines a function that clears erc track mode when it becomes too annoying, and sets up a global key binding for this function.
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)

;; Use UTF-8 encoding
(setq erc-server-coding-system '(utf-8 . utf-8))


;;automatic \away
(setq erc-autoaway-idle-time 600) ; Set idle time to 10 minutes
(setq erc-autoaway-use-emacs-idle t) ; Use Emacs idle time instead of ERC idle time
(setq erc-autoaway-message "I'm away") ; Set the away message
(add-hook 'erc-mode-hook 'erc-autoaway-mode) ; Enable automatic /away mode


;emoji
;(use-package emojify
;  :hook (after-init . global-emojify-mode))

