;; Functional Lisp Prettification (Consolidated mapping)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq prettify-symbols-alist 
                  '(("->" . ?→) ("<-" . ?←) ("=>" . ?⇒) ("<=" . ?≤) (">=" . ?≥)
                    ("!=" . ?≠) ("==" . ?≡) ("..." . ?…) ("~=" . ?≈)
                    ("lambda" . ?λ) ("&&" . ?∧) ("||" . ?∨) ("!" . ?¬) ("defun" . ?ƒ)))
            (prettify-symbols-mode 1)))
