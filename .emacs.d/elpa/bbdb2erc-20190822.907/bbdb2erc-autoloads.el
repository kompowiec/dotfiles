;;; bbdb2erc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "bbdb2erc" "bbdb2erc.el" (0 0 0 0))
;;; Generated autoloads from bbdb2erc.el

(autoload 'bbdb2erc-online-status "bbdb2erc" "\


\(fn &optional RECORD)" t nil)

(autoload 'bbdb2erc-pm "bbdb2erc" "\
Open up a chat with one of the entries in the irc-nick field
of the current BBDB record (or `record', if called
non-interactively), if one of those nicks is online in an ERC
server.

With a prefix argument (or if `prompt' is true), prompt for nick
and server. Otherwise, prioritise the first online nick in the
irc-nick field.

\(fn RECORD &optional PROMPT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "bbdb2erc" '("bbdb2erc-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; bbdb2erc-autoloads.el ends here
