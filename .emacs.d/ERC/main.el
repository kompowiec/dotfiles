'(erc-modules
		'(autoaway autojoin button completion dcc fill identd irccontrols list log match menu move-to-prompt netsplit networks noncommands notify notifications page readonly replace ring scrolltobottom services smiley sound stamp spelling track unmorse xdcc))

;; Rename server buffers to reflect the current network name instead of SERVER:PORT (e.g., "Libera.Chat" instead of "irc.libera.chat:6667").  This is useful when using a bouncer like ZNC where you have multiple connections to the same server.
(setq erc-rename-buffers t)

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; The following are commented out by default, but users of other non-Emacs IRC clients might find them useful.
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

(require 'erc-track)

(setq browse-url-browser-function 'eww-browse-url)

(require 'erc-image)
(add-to-list 'erc-modules 'image)
(setq erc-image-inline-resize-function 'erc-image-inline-resize-thumbnail)
(erc-update-modules)
