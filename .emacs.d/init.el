(load "~/.emacs.d/termux.el" nil t)
(load "~/.emacs.d/package-manager.el" nil t) ;; Set up package.el to work with MELP
(load "~/.emacs.d/evil.el" nil t) ;; Enable Evil
(load "~/.emacs.d/look.el" nil t)
(load "~/.emacs.d/org.el" nil t)
(load "~/.emacs.d/ERC/connect.el" nil t)
(load "~/.emacs.d/ERC/main.el" nil t)
(load "~/.emacs.d/ERC/stegtoy.el" nil t)
;(load "~/.emacs.d/eww.el" nil t)
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
 '(custom-enabled-themes '(the-matrix))
 '(custom-safe-themes
   '("d4c25f0a7a8cfd650071a678ee7881eb9d6cbd47c61f3cd397fdbd294ff8ca6f" "7e1b614af1e8e06d5dba84728088ac0dac9dcdc45407edf2bc0ccee0a8a7a9c8" "ae69a486b10ff74fc6eb24cb01793bb1092a951e90e42a1084e8ecaf8b9c5258" "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9" default))
 '(org-agenda-files nil)
 '(org-directory "/home/kompowiec/Org/")
 '(package-selected-packages
   '(the-matrix-theme org-roam-ui ## erc-image emojify magit flycheck org-roam evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
