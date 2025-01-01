'(erc-modules
	 '(autoaway autojoin button completion dcc fill identd irccontrols list log match menu move-to-prompt netsplit networks noncommands notify notifications page readonly replace ring scrolltobottom services smiley sound stamp spelling track unmorse xdcc))

;; Rename server buffers to reflect the current network name instead of SERVER:PORT (e.g., "Libera.Chat" instead of "irc.libera.chat:6667").  This is useful when using a bouncer like ZNC where you have multiple connections to the same server.
(setq erc-rename-buffers t)

;; The following are commented out by default, but users of other non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
;(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
;;(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
;;(setq erc-kill-server-buffer-on-quit t)

;This section sets up erc to highlight specific keywords and notify when mentioned in any channel.
(require 'erc-match)
(setq erc-keywords '("heniek heniu heniutek heniutku komowie komp komplikowiec kompo kompost kompot"))
(erc-match-mode)

(setq browse-url-browser-function 'eww-browse-url) ;run links in eww browser

;###LOOKS###
;siderbars
(require 'erc-track)
(require 'erc-status-sidebar) ;channels
(add-hook 'erc-mode-hook #'erc-status-sidebar-open)

;This section sets up erc to add a timestamp to each message.
(erc-timestamp-mode t)
(setq erc-timestamp-format "[%Y-%m-%d %H:%M:%S] ") ;YYYY-MM-DD HH:MM:SS
(setq erc-timestamp-only-if-changed-flag nil)  ;; Always show the timestamp
(defun my-erc-insert-timestamp-function (msg)
	(insert (format-time-string erc-timestamp-format (current-time)))) ;Insert timestamp at the beginning of the message.
(setq erc-insert-timestamp-function 'my-erc-insert-timestamp-function)

;in-line media
;May result in your IP address being revaled to external site operators
(require 'erc-image)
(add-to-list 'erc-modules 'image)
(setq erc-image-inline-resize-function 'erc-image-inline-resize-thumbnail)
(erc-update-modules)

;XEP-0392: Consistent Color Generation
;; Pool of colors to use when coloring IRC nicks.
(setq erc-colors-list '("green" "blue" "red"
												"dark gray" "dark orange"
												"dark magenta" "maroon"
												"indian red" "black" "forest green"
												"midnight blue" "dark violet"))
;; special colors for some people
(setq erc-nick-color-alist '(("John" . "blue")
														 ("Bob" . "red")
														 ))

(defun erc-get-color-for-nick (nick)
	"Gets a color for NICK. If NICK is in erc-nick-color-alist, use that color, else hash the nick and use a random color from the pool"
	(or (cdr (assoc nick erc-nick-color-alist))
			(nth
				(mod (string-to-number
							 (substring (md5 (downcase nick)) 0 6) 16)
						 (length erc-colors-list))
				erc-colors-list)))

(defun erc-put-color-on-nick ()
	"Modifies the color of nicks according to erc-get-color-for-nick"
	(save-excursion
		(goto-char (point-min))
		(if (looking-at "<\\([^>]*\\)>")
			(let ((nick (match-string 1)))
				(put-text-property (match-beginning 1) (match-end 1) 'face
													 (cons 'foreground-color
																 (erc-get-color-for-nick nick)))))))
(add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)


;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;###MISC###
; Auto-reconnect to the IRC server if disconnected
(setq erc-auto-reconnect t)
