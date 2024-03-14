;;; erc-crypt-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "erc-crypt" "erc-crypt.el" (0 0 0 0))
;;; Generated autoloads from erc-crypt.el

(autoload 'erc-crypt-enable "erc-crypt" "\
Enable PSK encryption for the current buffer." t nil)

(autoload 'erc-crypt-disable "erc-crypt" "\
Disable PSK encryption for the current buffer." t nil)

(autoload 'erc-crypt-set-key "erc-crypt" "\
Set `erc-crypt-key' for the current buffer.
The value used is the SHA1 hash of KEY.

\(fn KEY)" t nil)

(register-definition-prefixes "erc-crypt" '("erc-crypt-"))

;;;***

;;;### (autoloads nil nil ("erc-crypt-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; erc-crypt-autoloads.el ends here
