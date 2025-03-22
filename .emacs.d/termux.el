;;android look
(global-visual-line-mode 1)  ;; Enable word wrapping
(setq pixel-scroll-precision-use-momentum nil) ;; Disable momentum scrolling  
(pixel-scroll-precision-mode 1)  ;; Smooth scrolling  
(setq scroll-conservatively 101) ;; Prevent sudden jumps

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
