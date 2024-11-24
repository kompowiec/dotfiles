(setq gnus-permanently-visible-groups
      "^INBOX$\\|^Sent$") ;; If group is email INBOX or Sent
;;                           then always show it

(setq gnus-subscribe-options-newsgroup-method 'gnus-subscribe-killed
      gnus-browse-subscribe-newsgroup-method 'gnus-subscribe-zombies)

(setq gnus-extract-address-components 'mail-extract-address-components)

;;(setq bbdb-message-all-addresses t) ;; return all addresses of mail
;;(bbdb-initialize 'gnus 'message)
;;(bbdb-mua-auto-update-init 'gnus 'message)
;;(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq nnmail-expiry-wait 'immediate) ;; delete (expiry in gnus terms)
                                     ;; unwanted mail now

(define-key message-mode-map (kbd "<tab>")  ;;
  (lambda () (interactive)
    (message-tab) (message-tab) (other-window 1) (kill-buffer-and-window)))

(define-key gnus-article-mode-map (kbd "C-c in") ;; get new emails
  (lambda () (interactive) (other-window 1)      ;; or nntp posts
    (gnus-summary-insert-new-articles)))

(define-key gnus-summary-mode-map (kbd "C-c in")
  'gnus-summary-insert-new-articles)

;; GNUPG CONFIG
(epa-file-enable)
(setq mml2015-use 'epg
      epg-user-id '("A7AD0CE3D1D8D4A45BF973997F077793FD4A8CDA")
      mml-secure-openpgp-signers '("A7AD0CE3D1D8D4A45BF973997F077793FD4A8CDA")
      
      mml-secure-openpgp-always-trust nil
      mml-secure-openpgp-sign-with-sender t
      
      gnus-treat-x-pgp-sig t
      mml-secure-openpgp-encrypt-to-self t

      mm-verify-option 'always
      mm-decrypt-option 'always
      mm-sign-option nil ;<-----+---- don't ask about
      mm-encrypt-option nil ;<--+^    keys every time

      gnus-buttonized-mime-types
      '("multipart/alternative"
        "multipart/encrypted"
        "multipart/signed"))

(setq gnus-use-cache t)

;; MAIL CONFIG

;; receiving
(setq user-mail-address email-address
      user-full-name "edzio listonosz"
      gnus-select-method '(nnml "poczta.o2.pl"
                                (nnml-directory "~/Mail")
                                (nnml-active-file "~/Mail/active")
                                (nnml-newsgroups-file "~/Mail/newsgroups")
                                (nnml-nov-is-evil nil)
                                (nnml-nov-file-name ".overview")
                                ;;(nnml-prepare-save-mail-hook
                                (nnml-use-compressed-files nil)))


(defun fetchmail-fetch-email ()
  "Fetch email."
  (interactive)
  (call-process "fetchmail" nil nil nil ))


(define-key gnus-group-mode-map (kbd "C-c f") (fetchmail-fetch-email))
(define-key gnus-server-mode-map (kbd "C-c f") (fetchmail-fetch-email))
(define-key gnus-summary-mode-map (kbd "C-c f") (fetchmail-fetch-email))
(define-key gnus-browse-mode-map (kbd "C-c f") (fetchmail-fetch-email))
  

;; sending
(setq send-mail-function          'msmtp-send-email
      message-send-mail-function  'msmtp-send-email)

(defun msmtp-send-email ()
  ""
  (let* ((charnum    (re-search-forward "To:\s"))
         (Recipent   (progn (goto-char charnum)
                            (buffer-substring-no-properties
                             charnum (line-end-position))))
         (charnum    (re-search-forward "Subject:\s"))
         (Subject    (progn (goto-char charnum)
                            (buffer-substring-no-properties
                             charnum (line-end-position))))
         (charnum    (re-search-forward "--text follows this line--"))
         (beg        charnum)
         (end        (point-max))
         (msg        (buffer-substring-no-properties charnum (point-max))))
    (with-temp-buffer (insert msg)
                      (goto-char (point-min))
                      (insert (concat "Subject: " Subject "\n\n"))
                      (setq end (+ end (length Subject) 11))
                      (call-process-region
                       (point-min) (point-max) "/usr/bin/msmtp"
                       t '(t nil) nil "-a" "default" "-t" Recipent))))
       
(defun gnus-goto-inbox-group ()
  ""
  (interactive)
  (cond ((eq major-mode 'gnus-group-mode)
         (gnus-group-enter-server-mode)
         (gnus-server-goto-server "nnml:poczta.o2.pl")
         (gnus-server-read-server-in-server-buffer "nnml:poczta.o2.pl"))
        ((eq major-mode 'gnus-browse-mode)
         (gnus-browse-exit)
         (gnus-server-goto-server "nmml:poczta.o2.pl")
         (gnus-server-read-server-in-server-buffer "nnml:poczta.o2.pl"))
        ((eq major-mode 'gnus-server-mode)
         (gnus-browse-exit)
         (gnus-server-goto-server "nnml:poczta.o2.pl")
         (gnus-server-read-server-in-server-buffer "nnml:poczta.o2.pl"))))


(define-key gnus-group-mode-map (kbd "v j") (gnus-goto-inbox-group))
(define-key gnus-server-mode-map (kbd "v j") (gnus-goto-inbox-group))
(define-key gnus-summary-mode-map (kbd "v j") (gnus-goto-inbox-group))
(define-key gnus-browse-mode-map (kbd "v j") (gnus-goto-inbox-group))


(provide 'gnus-initrc)
;;; gnus-initrc.el ends here
