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

(setq Man-notify-method 'pushy
			winum-scope 'frame-local)
(defun winum-replace-window (window-number key-sequence)
	(interactive "P\nkKey sequence: ")
	(let ((sym (key-binding key-sequence))
				(num (if window-number window-number (winum-get-number))))
		(winum-select-window-by-number num)
		(call-interactively sym)))

(defun kill-buffer-ext (&optional arg)
	"When ARG is t - prompt for buffer to kill.  Otherwise kill current buffer."
	(interactive "P")
	(if (eq arg nil)
		(kill-this-buffer)
		(call-interactively 'kill-buffer)))
(global-set-key (kbd "C-x k") 'kill-buffer-ext)

(defun my-set-theme-based-on-size ()
  "Change theme to 'notink if the initial frame size is 74x24."
  (when (and (= (frame-width) 74)
             (= (frame-height) 24))
    (load-theme 'notink t)))

;; Run this function after Emacs starts
(add-hook 'emacs-startup-hook #'my-set-theme-based-on-size)
