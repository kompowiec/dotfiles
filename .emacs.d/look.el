(setq inhibit-startup-message t) ;;disable splash screen and startup message
(setq initial-scratch-message nil)
(setq visible-bell 1)

(require 'use-package)
(setq use-package-always-ensure t)

;(require 'doom-modeline)
;(doom-modeline-mode 1)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;(if (window-system)
;  (set-frame-height (selected-frame) 29))

(toggle-frame-maximized)

(require 'yasnippet)
(yas-global-mode 1)
