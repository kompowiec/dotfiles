(load "~/.emacs.d/desktop/package-manager.el" nil t) ;; Set up package.el to work with MELP
(load "~/.emacs.d/desktop/look.el" nil t)
(load "~/.emacs.d/desktop/prettify.el" nil t)
(load "~/.emacs.d/desktop/org.el" nil t)
;ERC
;;LOAD
(load "~/.emacs.d/desktop/ERC/connect.el" nil t)
(load "~/.emacs.d/desktop/ERC/main.el" nil t)
(load "~/.emacs.d/desktop/ERC/stegtoy.el" nil t)
(load "~/.emacs.d/desktop/jabber.el" nil t)

;(load "~/.emacs.d/desktop/gnus.el" nil t) ;e-mail
;(load "~/.emacs.d/desktop/syntax/cbot-mode.el" nil t)

;XMPP
;(load "~/.emacs.d/desktop/XMPP/jabber-pgp.el" nil t)
;(load "~/.emacs.d/desktop/XMPP/emacs-jabber-ox-mode/jabber-ox.el" nil t)
;(load "~/.emacs.d/desktop/XMPP/emacs-jabber-ox-mode/jabber-pep.el" nil t)
;(add-to-list 'load-path "~/.emacs.d/desktop/XMPP/emacs-jabber-ox-mode/") (require 'jabber-ox)
;(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/LOCAL/jabber/ox"))
;(require 'jabber-ox)
;(setq jabber-ox-gpg-name-real "xmpp:kompowiec2@jabbim.cz")
;(setq jabber-debug-log-xml t)

;; --- System Custom Blocks (Kept unified at bottom) ---
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f"
    "#f6f3e8"])
 '(custom-enabled-themes '(material))
 '(custom-safe-themes
   '("90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940"
     "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a"
     default))
 '(erc-ask-about-multiline-input t)
 '(erc-speedbar-sort-users-type 'activity)
 '(jabber-ox-gpg-name-real "xmpp:kompowiec2@jabbim.cz")
 '(jabber-pgp-gpg-name-address "test")
 '(jabber-pgp-gpg-name-real "test")
 '(minimap-automatically-delete-window nil)
 '(minimap-dedicated-window nil)
 '(minimap-mode t)
 '(org-agenda-files (list "/home/kompowiec/Org/"))
 '(org-directory "/home/kompowiec/Org/")
 '(package-selected-packages
   '(any-buffer-toggle emojify erc erc-image erc-status-sidebar flycheck
		       imgur.el jabber jabber-ox-mode magit
		       material-theme mediawiki mediawiki-el org-roam
		       org-roam-ui undo-tree))
 '(package-vc-selected-packages
   '((material-theme :vc-backend Git :url
		     "https://github.com/cpaulik/emacs-material-theme")
     (mediawiki-el :vc-backend Git :url
		   "https://github.com/hexmode/mediawiki-el")
     (erc-status-sidebar :vc-backend Git :url
			 "https://github.com/drewbarbs/erc-status-sidebar.git")
     (jabber-ox-mode :vc-backend Git :url
		     "https://codeberg.org/eddy97/emacs-jabber-ox-mode")
     (any-buffer-toggle :vc-backend Git :url
			"https://codeberg.org/eddy97/emacs-any-buffer-toggle"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "JB  " :slant normal :weight regular :height 120 :width normal)))))

