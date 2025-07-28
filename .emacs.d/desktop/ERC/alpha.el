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

;TODO: short url - tinyurl

;TODO: send audio - vocaroo

;TODO take a photo - imgur

;TODO: Post a text snippet - pastebin

;TODO: delay message

;TODO: delete polish diacritic signs

;TODO display the joined and quit messages, aggregating users into one line

(defun process-erc-log (log)
  "Process the given ERC log, aggregating join and quit messages into a single line."
  (let ((joins '())
        (quits '())
        (lines (split-string log "\n")))
    ;; Process each line of the log
    (dolist (line lines)
      (cond
       ;; Match and extract nicknames for join messages
       ((string-match "→ \\([^ ]+\\) joined" line)
        (push (match-string 1 line) joins))
       ;; Match and extract nicknames for quit messages
       ((string-match "⇐ \\([^ ]+\\) quit" line)
        (push (match-string 1 line) quits))))
    ;; Format and return the output
    (concat "→ " (mapconcat 'identity (reverse joins) ", ")
            " joined  ⇐ " (mapconcat 'identity (reverse quits) ", ")
            " quit")))

;; Example log input
(let ((log "14:52:52 ⇐ sdfg quit (~pirc@911C409D.671D366A.A0157863.IP) Quit: Użytkownik zamknął stronę
14:53:30 → edzia joined (~pirc@vhost:gruba.dupa)
14:53:50 → trevor764 joined (~pirc@ukryty-4A0D68AA.dip0.t-ipconnect.de)
14:53:56 ⇐ trevor763 quit (~pirc@ukryty-4BD13605.dip0.t-ipconnect.de) Ping timeout: 100 seconds
14:54:37 ⇐ Abi quit (~pirc@ukryty-DCCABCB8.dip0.t-ipconnect.de) Quit: Użytkownik rozłączył się
14:54:48 → trevor765 joined (~pirc@ukryty-2643252.dip0.t-ipconnect.de)
14:55:04 → zolwik joined (~pirc@2C40BE8D.16EEE530.4ADB7D4B.IP)"))

  ;; Process the log and print the output
  (message "%s" (process-erc-log log)))

