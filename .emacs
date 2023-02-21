;main
(load "~/.emacs.d/package-manager.el" nil t)
(load "~/.emacs.d/apperance.el" nil t)
(load "~/.emacs.d/built-in/org-mode.el" nil t)
; (load "~/.emacs.d/built-in/dired.el" nil t)
;apps
; (load "~/.emacs.d/Apps/Others/Desktop environments/exwm.el" nil t)
(load "~/.emacs.d/plugins/vim.el" nil t)
; (load "~/.emacs.d/Apps/Internet/Communication/jabber.el" nil t)
; (load "~/.emacs.d/Apps/Internet/Communication/matrix-client.el" nil t)
; (load "~/.emacs.d/Apps/Internet/News, RSS, and blogs/News aggregators/elfeed.el" nil t) 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wheatgrass))
 '(erc-modules
   '(autoaway autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring services smiley stamp track))
 '(org-hide-block-startup t)
 '(package-selected-packages
   '(chess mediawiki color-theme-sanityinc-tomorrow markdown-mode elpher emacsql-sqlite-module emacsql-sqlite f ledger-mode exwm evil)))
; org:
;; ledger-mode - Helper code for use with the "ledger" command-line tool 
; org-roam:
;; emacsql - High-level SQL database front-end 
;; emacsql-sqlite -EmacSQL back-end for SQLite 
;; emacsql-sqlite-module - EmacSQL back-end for SQLite using a module 
;; dash - A modern list library for Emacs 
;; magit-section - Sections for read-only buffers. 
;other
;; elpher - A friendly gopher and gemini client
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
