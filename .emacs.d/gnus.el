;; Primary method: IMAP
(setq gnus-select-method
      '(nnimap "poczta.o2.pl"
               (nnimap-address "poczta.o2.pl")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; Secondary methods: POP3 (if needed)
(setq gnus-secondary-select-methods
      '((nnpop3 "poczta.o2.pl"
                (nnpop3-address "poczta.o2.pl")
                (nnpop3-port 995)
                (nnpop3-stream ssl))))

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

;; Enable the configuration
(gnus)
