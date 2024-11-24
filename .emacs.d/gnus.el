;usenet
(defun usenet-connect ()
;; Set your primary news server
(setq gnus-select-method '(nntp "news.your-news-server.com"))

;; Optionally, add secondary servers
 (setq gnus-secondary-select-methods '((nntp "news.server2.com")
                                       (nntp "news.server3.com"))))
;e-mail
(defun gnus-connect ()
  "Connect to GNUS using the specified IMAP and SMTP settings."
  (interactive)
  
  ;; Primary method: IMAP
  (setq gnus-select-method
        '(nnimap "poczta.o2.pl"
                 (nnimap-address "poczta.o2.pl")
                 (nnimap-server-port 993)
                 (nnimap-stream ssl)))
  ;; SMTP configuration for sending mail
  (setq smtpmail-smtp-server "poczta.o2.pl"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl)
  ;; Enabling SMTP authentication using .authinfo
  (setq smtpmail-auth-credentials "~/.authinfo"
        smtpmail-starttls-credentials '(("poczta.o2.pl" 465 nil nil))
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it)
  ;; Set the user email address and name
  (setq user-mail-address "diasp@o2.pl"
        user-full-name "diasp")
  ;; Optional: If you want to use the same folder for sent mails for both configurations
  (setq gnus-message-archive-method '(nnimap "poczta.o2.pl")
        gnus-message-archive-group "Sent")
  ;; Optional: Customize the Gnus user interface
  (setq gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f %B%s%)\n"
        gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
        gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)\n"
        gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
        gnus-thread-sort-functions '(gnus-thread-sort-by-date))
  ;; Start Gnus
  (gnus))

;;RSS
(require 'nnrss)
(defun rss-connect ()
(setq gnus-secondary-select-methods
      '((nnrss "")))
;; Optionally set the directory to store your RSS articles
(setq nnrss-directory "~/rss/"))
;; Optional: Update interval for RSS feeds (in seconds)
;; (setq nnrss-interval 3600) ; Refresh every hour

;; Ensure the function is available for M-x
(autoload 'gnus-connect "gnus-connect" "Connect to GNUS using the specified settings." t)
