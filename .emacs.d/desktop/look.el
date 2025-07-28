(setq inhibit-startup-message t) ;;disable splash screen and startup message
(setq initial-scratch-message nil)
(setq visible-bell 1)
(global-visual-line-mode t) ; line wrap

(require 'use-package)
(setq use-package-always-ensure t)

(use-package flycheck
						 :ensure t
						 :init (global-flycheck-mode))

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
