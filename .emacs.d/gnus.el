(require 'gnus)

(setq gnus-secondary-select-methods '((nnml "")
									  (nnfolder "local"
												(directory
												 "/home/kompowiec/Mail/mbox")
												(get-new-mail nil)
												(file-name-transform-function
												 (lambda (file-name)
												   (concat "/" file-name))))))
									  
(setq mail-sources
	  '((file :path "/home/kompowiec/Mail/mbox")))

(provide 'email-conf)

