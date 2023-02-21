;colorscheme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wheatgrass))
 '(package-selected-packages
   '(emacsql-sqlite-module magit-section emacsql-sqlite org-roam-ui magit f exwm evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)
(setq visible-bell 1)
