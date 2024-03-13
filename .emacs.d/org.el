;Automatic Item TODO/DOING/DONE State Transitions for Checkbox Changes 
(setq org-todo-keywords
			'((sequence "TODO" "NEXT" "|" "DONE" )))

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

;org
(custom-set-variables
 '(org-directory "/home/kompowiec/Org/")
 '(org-agenda-files (list org-directory)))

;org-roam
(use-package org-roam
						 :ensure t
						 :custom
						 (org-roam-directory (file-truename "/home/kompowiec2/Org"))
						 :bind (("C-c n l" . org-roam-buffer-toggle)
										("C-c n f" . org-roam-node-find)
										("C-c n g" . org-roam-graph)
										("C-c n i" . org-roam-node-insert)
										("C-c n c" . org-roam-capture)
										;; Dailies
										("C-c n j" . org-roam-dailies-capture-today))
						 :config
						 (org-roam-setup))
(setq find-file-visit-truename t)
(org-roam-db-autosync-mode)

;duplicate
(defun collect-duplicate-headings ()
  (let (hls dups)
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward org-complex-heading-regexp nil t)
        (let* ((el (org-element-at-point))
               (hl (org-element-property :title el))
               (pos (org-element-property :begin el)))
          (push (cons hl pos) hls)))
      (dolist (hl hls)
        (when (> (cl-count (car hl) (mapcar #'car hls)
                           :test 'equal)
                 1)
          (push hl dups)))
      (nreverse dups))))
