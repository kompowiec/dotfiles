;Automatic Item TODO/DOING/DONE State Transitions for Checkbox Changes 
(setq org-todo-keywords
			'((sequence "TODO" "NEXT" "REM" "|" "DONE" "DELEGATED" "DID")))

(setq org-agenda-include-diary t) ; diary
(global-visual-line-mode t) ; line wrap

; You should have aspell-pl and aspell-en packages installed
(let ((langs '("english" "polish")))
	(setq lang-ring (make-ring (length langs)))
	(dolist (elem langs) (ring-insert lang-ring elem)))
(defun cycle-ispell-languages ()
	(interactive)
	(let ((lang (ring-ref lang-ring -1)))
		(ring-insert lang-ring lang)
		(ispell-change-dictionary lang)))

(global-set-key (kbd "C-1") 'cycle-ispell-languages)

;
(setq org-agenda-files '("/home/kompowiec/Org/"))

;org-roam
(setq org-roam-directory (file-truename "~/Org"))
