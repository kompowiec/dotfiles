;; Install Evil and disable C-i to jump forward to restore TAB functionality in Org mode.
(unless (package-installed-p 'evil)
  (package-install 'evil))

(use-package evil
  :init
  (setq evil-want-C-i-jump nil)  ;; Disable the C-i jump functionality
  :config
  (evil-mode 1))

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

