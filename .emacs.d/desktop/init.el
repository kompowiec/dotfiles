(load "~/.emacs.d/desktop/package-manager.el" nil t) ;; Set up package.el to work with MELP
;(load "~/.emacs.d/evil.el" nil t) ;; Enable Evil
(load "~/.emacs.d/desktop/look.el" nil t)
(load "~/.emacs.d/desktop/org.el" nil t)
;ERC
;;LOAD
(load "~/.emacs.d/desktop/ERC/connect.el" nil t)
(load "~/.emacs.d/desktop/ERC/main.el" nil t)
(load "~/.emacs.d/desktop/ERC/stegtoy.el" nil t)

;(load "~/.emacs.d/desktop/gnus.el" nil t)
(load "~/.emacs.d/desktop/optimize.el" nil t)
;(load "~/.emacs.d/desktop/syntax/cbot-mode.el" nil t)

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
 '(custom-enabled-themes '(wheatgrass))
 '(erc-ask-about-multiline-input t)
 '(erc-speedbar-sort-users-type 'activity)
 '(package-selected-packages
   '(## emojify erc erc-image flycheck imgur.el magit org-roam
	org-roam-ui))
 '(package-vc-selected-packages
   '(((erc-status-sidebar.el :vc-backend Git :url
			     "https://github.com/drewbarbs/erc-status-sidebar.git")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
