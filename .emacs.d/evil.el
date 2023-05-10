(require 'evil)
(defun toggle-evil-mode ()
	"Toggle evil mode."
	(interactive)
	(if (bound-and-true-p evil-mode)
		(evil-mode 0)
		(evil-mode 1)))
(defun add-toggle-evil-mode-to-options-menu ()
	"Add toggle evil mode to options menu."
	(define-key menu-bar-options-menu [toggle-evil-mode]
							`(menu-item ,(if (bound-and-true-p evil-mode)
														 "Disable Evil Mode"
														 "Enable Evil Mode")
													toggle-evil-mode
													:help ,(if (bound-and-true-p evil-mode)
																	 "Turn off Evil Mode"
																	 "Turn on Evil Mode"))))
(add-toggle-evil-mode-to-options-menu)
(setq evil-want-C-i-jump nil) ;Tab functionality back
