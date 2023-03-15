;; Load ERC
(require 'erc)

(setq erc-server-list
      '(("irc.pirc.pl"
         :port 6667
         :nick "kompowiec2"
         :password "xxx")
        ("irc."
         :port 6667
         :nick "my-nick2"
         :password "my-password2")))

;; Set the ERC user information
(setq erc-user-full-name "My Full Name")
(setq erc-user-information
      '((("irc.pirc.pl" :nick "my-nick1")
         "My User Information 1")
        (("irc.example.com" :nick "my-nick2")
         "My User Information 2")))

;; Auto-join channels
(setq erc-autojoin-channels-alist
      '(("irc.freenode.net" "#my-channel1" "#my-channel2")))

;; Auto-reconnect to the IRC server if disconnected
(setq erc-auto-reconnect t)

;; Use SSL to connect to the IRC server
(setq erc-secure-server-list
      '(
        ("irc.pirc.pl" 6697)
        ))

;; Use UTF-8 encoding
(setq erc-server-coding-system '(utf-8 . utf-8))

;; Set ERC to use SASL authentication
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil
      erc-nickserv-passwords
      `((freenode (("my-nick" . "my-password")))))

;; Set the logging directory
(setq erc-log-channels-directory "~/.emacs.d/erc/logs/")

;; Enable logging of channels
(setq erc-log-channels-directory "~/.emacs.d/erc/logs/")
(setq erc-log-insert-log-on-open t)
(setq erc-log-channels t)
(setq erc-log-file-coding-system 'utf-8)

;; Set ERC notifications
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
(setq erc-track-showcount t)
(setq erc-track-shorten-start 5)

;; Show inline images in ERC
(add-to-list 'erc-modules 'image)
(add-to-list 'erc-image-inline-rescale-types "image/png")
(add-to-list 'erc-image-inline-rescale-types "image/jpeg")
(add-to-list 'erc-image-inline-rescale-types "image/gif")
(add-to-list 'erc-image-inline-rescale-types "image/webp")
(add-to-list 'erc-image-inline-rescale-types "image/bmp")
(add-to-list 'erc-image-inline-rescale-types "image/tiff")

;;automatic \away
(require 'erc-idle)

(setq erc-autoaway-idle-time 600) ; Set idle time to 10 minutes
(setq erc-autoaway-use-emacs-idle t) ; Use Emacs idle time instead of ERC idle time
(setq erc-autoaway-message "I'm away") ; Set the away message

(add-hook 'erc-mode-hook 'erc-autoaway-mode) ; Enable automatic /away mode

