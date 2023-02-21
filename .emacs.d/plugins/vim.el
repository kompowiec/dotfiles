  (setq evil-want-C-i-jump nil) ;Tab functionality back
  (require 'evil)
  (evil-mode 1)
(add-hook 'after-init-hook #'doom-modeline-mode)
