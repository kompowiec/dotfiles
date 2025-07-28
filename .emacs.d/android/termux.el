;; REMEMBER, you need special version termux which share emacs UID!
;;https://sourceforge.net/projects/android-ports-for-gnu-emacs/files/termux/
;;termux shell
(setenv "SHELL" "/data/data/com.termux/files/usr/bin/bash")
(setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
											 (getenv "PATH")))
(setenv "LD_LIBRARY_PATH" (format "%s:%s"
																	"/data/data/com.termux/files/usr/lib"
																	(getenv "LD_LIBRARY_PATH")))
(push "/data/data/com.termux/files/usr/bin" exec-path)

;TODO
;;error: Eager macro-expansion failure: (error "Shortdoc f function ‘f-older
;;«-p’: bad keyword ‘:noeval’")
