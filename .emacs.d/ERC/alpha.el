;limit characters
(defun check-erc-limit ()
  "Check if the text in the current buffer exceeds the ERC limit of 510 characters."
  (interactive)
  (if (> (line-end-position) 510)
      (message "ERC limit exceeded!")
    (message "Within ERC limit.")))

;http upload
;;curl required
(defun http-upload-file (filename)
  "Upload FILENAME to transfer.sh and copy the URL to the clipboard."
  (interactive "fFile to upload: ")
  (let ((url (shell-command-to-string (concat "curl --upload-file " (shell-quote-argument filename) " https://transfer.sh/" (shell-quote-argument (file-name-nondirectory filename)))))) 
    (kill-new url)
    (message "Uploaded %s to %s" filename url)))

;;To use the http-upload-file function, simply call it with the path to the file to upload and the URL of the web server:
(global-set-key (kbd "C-c t") 'http-upload-file)

  "Check if the text in the current buffer exceeds the ERC limit of 510 characters."
  (interactive)
  (if (> (line-end-position) 510)
      (message "ERC limit exceeded!")
    (message "Within ERC limit."))

;Embed 3rd party images
;May result in your IP address being revaled to external site operators
(add-to-list 'erc-modules 'image)
(add-to-list 'erc-image-inline-rescale-types "image/png")
(add-to-list 'erc-image-inline-rescale-types "image/jpeg")
(add-to-list 'erc-image-inline-rescale-types "image/gif")
(add-to-list 'erc-image-inline-rescale-types "image/webp")
(add-to-list 'erc-image-inline-rescale-types "image/bmp")
(add-to-list 'image-auto-resize-on-window-resize "image/tiff')

;TODO: short url - tinyurl

;TODO: send audio - vocaroo

;TODO take a photo - imgur

;TODO: Post a text snippet - pastebin

;TODO: delay message

;TODO: delete polish diacritic signs
