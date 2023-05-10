;; Load ERC on startup
(add-hook 'erc-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)
            (erc-timestamp-mode t)
            (erc-spelling-mode t)))

;; Set up keybindings
(add-hook 'erc-mode-hook
          (lambda ()
            (define-key erc-mode-map (kbd "C-c C-k") 'erc-quit-server)))

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

(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))

;This section configures erc to fill lines that go beyond a specified column limit.
(require 'erc-fill)
(erc-fill-mode t)

(require 'erc-ring)
(erc-ring-mode t)

;This section sets up erc to track and notify when a channel is affected by a netsplit.
(require 'erc-netsplit)
(erc-netsplit-mode t)

;This section sets up erc to add a timestamp to each message.
(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

;This section disables erc button mode, which can cause erc to become slow when the buffer has many links.
(erc-button-mode nil) ;slow

;This section sets the user's full name and email address for erc.
(setq erc-user-full-name "David McCabe")
(setq erc-email-userid "david@dmccabe.0rg")

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

;;; Filter bots/users - requires lexical binding.
(defun squiddo-erc-filter-bot (channel bots)
  "Filter messages to/from bots.
CHANNEL is the name of the channel to watch.
BOTS are a list of bots (nicks) to filter."
  (message "Filtering %s channel for %s bots" channel bots)
  (let ((bot-list `()))
    (dolist (bot bots)
      (push (format "%s:" bot) bot-list)
      (push (format "<%s>" bot) bot-list))
    (message "Bot list %s" bot-list)
    (add-hook 'erc-insert-pre-hook (function (lambda (msg)
                                               (when (string-match (buffer-name) channel)
                                                 (dolist (bot-indicator bot-list)
                                                   (when (string-match bot-indicator msg)
                                                     ;(message "Filtering bot: %s" msg)
                                                     (setq erc-insert-this nil)))))))))

(squiddo-erc-filter-bot "#emacs" '("foo" "bar" "baz"))
(squiddo-erc-filter-bot "#lisp" '("foo" "bar" "baz"))

;; Use UTF-8 encoding
(setq erc-server-coding-system '(utf-8 . utf-8))

; Embed 3rd party images
;May result in your IP address being revaled to external site operators
;(add-to-list 'erc-modules 'image)
;(add-to-list 'erc-image-inline-rescale-types "image/png")
;(add-to-list 'erc-image-inline-rescale-types "image/jpeg")
;(add-to-list 'erc-image-inline-rescale-types "image/gif")
;(add-to-list 'erc-image-inline-rescale-types "image/webp")
;(add-to-list 'erc-image-inline-rescale-types "image/bmp")
;(add-to-list 'image-auto-resize-on-window-resize "image/tiff')

;limit characters
(defun check-erc-limit ()
  "Check if the text in the current buffer exceeds the ERC limit of 510 characters."
  (interactive)
  (if (> (line-end-position) 510)
      (message "ERC limit exceeded!")
    (message "Within ERC limit.")))

;; Set ERC notifications
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-showcount t)
(setq erc-track-shorten-start 5)

;;automatic \away
(setq erc-autoaway-idle-time 600) ; Set idle time to 10 minutes
(setq erc-autoaway-use-emacs-idle t) ; Use Emacs idle time instead of ERC idle time
(setq erc-autoaway-message "I'm away") ; Set the away message
(add-hook 'erc-mode-hook 'erc-autoaway-mode) ; Enable automatic /away mode

;http upload
;;curl required
(defun http-upload-file (filename)
  "Upload FILENAME to transfer.sh and copy the URL to the clipboard."
  (interactive "fFile to upload: ")
  (let ((url (shell-command-to-string (concat "curl --upload-file " (shell-quote-argument filename) " https://transfer.sh/" (shell-quote-argument (file-name-nondirectory filename)))))) 
    (kill-new url)
    (message "Uploaded %s to %s" filename url)))

;;To use the http-upload-file function, simply call it with the path to the file to upload and the URL of the web server:
(global-set-key (kbd "C-c t") 'http-upload-file)

  "Check if the text in the current buffer exceeds the ERC limit of 510 characters."
  (interactive)
  (if (> (line-end-position) 510)
      (message "ERC limit exceeded!")
    (message "Within ERC limit."))

; Auto-reconnect to the IRC server if disconnected
(setq erc-auto-reconnect t)

;TODO: short url - tinyurl

;TODO: send audio - vocaroo

;TODO take a photo - imgur

;TODO: Post a text snippet - pastebin

