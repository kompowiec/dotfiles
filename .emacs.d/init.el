(load "~/.emacs.d/package-manager.el" nil t) ;; Set up package.el to work with MELPA
(load "~/.emacs.d/evil.el" nil t) ;; Enable Evil
(load "~/.emacs.d/look.el" nil t)
(load "~/.emacs.d/org.el" nil t)
; (load "~/.emacs.d/erc.el" nil t)
;;(load "~/.emacs.d/gnus.el" nil t)
(load "~/.emacs.d/optimize.el" nil t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes '(wheatgrass))
 '(org-agenda-files (list org-directory))
 '(org-directory "/home/kompowiec/Org/")
 '(package-selected-packages '(flycheck org-roam doom-modeline evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
