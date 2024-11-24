;Automatic Item TODO/DOING/DONE State Transitions for Checkbox Changes 
(setq org-todo-keywords
			'((sequence "TODO" "FAIL" "|" "DONE" )))

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
	;; Initialize empty lists to store headings and duplicates
	(let (hls dups)
		;; Save the current position to return to it later
		(save-excursion
			;; Move to the end of the buffer
			(goto-char (point-max))
			;; Search backward for Org mode headings using the complex heading regexp
			(while (re-search-backward org-complex-heading-regexp nil t)
						 ;; Extract the current element and its properties
						 (let* ((el (org-element-at-point))
										(hl (org-element-property :title el))
										(pos (org-element-property :begin el)))
							 ;; Store the heading and its position in the list
							 (push (cons hl pos) hls)))
			;; Iterate over the list of headings
			(dolist (hl hls)
				;; Check if the current heading appears more than once
				(when (> (cl-count (car hl) (mapcar #'car hls)
													 :test 'equal)
								 1)
					;; If it does, add it to the duplicates list
					(push hl dups)))
			;; Return the list of duplicates in reverse order
			(nreverse dups))))

