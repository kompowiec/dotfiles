;; https://github.com/jwiegley/use-package
(eval-when-compile
	(require 'use-package))
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
						 '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(add-to-list 'package-archives
						 (cons "nongnu" (format "http%s://elpa.nongnu.org/nongnu/"
																		(if (gnutls-available-p) "s" ""))))
