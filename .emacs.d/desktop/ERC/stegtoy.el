(require 'erc)
(require 'erc-fill)

(setq erc-stegtoy-colors
      '(("a" . "\x03\x030")
        ("b" . "\x03\x031")
        ("c" . "\x03\x032")
        ("d" . "\x03\x033")
        ("e" . "\x03\x034")
        ("f" . "\x03\x035")
        ("g" . "\x03\x036")
        ("h" . "\x03\x037")
        ("i" . "\x03\x038")
        ("j" . "\x03\x039")
        ("k" . "\x03\x0310")
        ("l" . "\x03\x0311")
        ("m" . "\x03\x0312")
        ("n" . "\x03\x0313")
        ("o" . "\x03\x0314")
        ("p" . "\x03\x0315")
        ("q" . "\x03\x030,01")
        ("r" . "\x03\x030,02")
        ("s" . "\x03\x030,03")
        ("t" . "\x03\x030,04")
        ("u" . "\x03\x030,05")
        ("v" . "\x03\x030,06")
        ("w" . "\x03\x030,07")
        ("x" . "\x03\x030,08")
        ("y" . "\x03\x030,09")
        ("z" . "\x03\x030,10")
        ("_" . "\x03\x030,11")))


(setq separators (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                       "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                       " "))

(setq codes (list "\x03\x030"
                   "\x03\x031"
                   "\x03\x032"
                   "\x03\x033"
                   "\x03\x034"
                   "\x03\x035"
                   "\x03\x036"
                   "\x03\x037"
                   "\x03\x038"
                   "\x03\x039"
                   "\x03\x0310"
                   "\x03\x0311"
                   "\x03\x0312"
                   "\x03\x0313"
                   "\x03\x0314"
                   "\x03\x0315"
                   "\x03\x030,01"
                   "\x03\x030,02"
                   "\x03\x030,03"
                   "\x03\x030,04"
                   "\x03\x030,05"
                   "\x03\x030,06"
                   "\x03\x030,07"
                   "\x03\x030,08"
                   "\x03\x030,09"
                   "\x03\x030,10"
                   "\x03\x030,11"))

(defun erc-stegtoy-encode (string cover-message)
  (let* ((len           (length string)) 
         (cover-chars   (split-string cover-message "" t)) ;; split cover msg into chars
         (message-chars (split-string string "" t))  ;; split message into chars
         (encoded       "")
         (encoded-diff  "")
         (diff          (- (length cover-message) len)) ;; get free space in cover msg
         (diff-cover    (nthcdr diff cover-chars)) ;; get space to pad
         (diff-random   (random-ascii-chars diff)) ;; get padding
         (diff-split    (split-string diff-random "" t))) ;; padding to pad free space


    (while cover-chars
      ;;(setq len (- diff 1))
      (let* ((char-msg        (car message-chars))  ;; first char of original msg
             (encoded-char    (cdr (assoc-string char-msg erc-stegtoy-colors))) ;; encoded first char of msg
             (char-cover      (car cover-chars)))   ;; first char of cover-message
                              
        (setq message-chars   (cdr message-chars)
              cover-chars     (cdr cover-chars))
                            
        (setq encoded (concat encoded (concat encoded-char char-cover)))))
    encoded))

    ;; (while (>= diff 1)
    ;;   (setq diff (- diff 1))
    ;;   (let* ((char-diff       (car diff-split))
    ;;          (encoded-cover   (cdr (assoc-string char-diff COLORS)))
    ;;          (char-cover      (car diff-cover)))

    ;;     (setq  diff-cover     (cdr diff-cover)
    ;;            diff-split     (cdr diff-split))

    ;;     (setq encoded-diff (concat encoded-diff (concat encoded-cover char-cover)))))

    ;; (setq result (concat encoded encoded-diff))))

(defun random-ascii-chars (len)
  (setq chars "")
  (while (>= len 1)
    (setq chars
          (concat (char-to-string
                   (+ 97 (random (- 122 97))))
                  chars))
        (setq len (- len 1)))
  chars)

(defun erc-stegtoy-decode (string)
  (let ((message (split-string string (regexp-opt separators) t))
        (decoded ""))

    (while message
      (let ((char (rassoc (car message) erc-stegtoy-colors)))
        (setq decoded (concat decoded (car char))))
      (setq message (cdr message)))
    decoded))

(defun erc-stegtoy-msg (string)
  (let* ((no-prop (with-temp-buffer
                    (insert string)
                    (set-text-properties (point-min) (point-max) nil)
                    (buffer-string)))
         (split   (split-string no-prop " "))
         (nick    (car split))
         (msg     (mapconcat #'identity (cdr split) " ")))
    (message (format "erc-stegtoy: msg %s %s" nick (erc-stegtoy-decode msg)))))
     
(defun erc-cmd-STEGTOY (status)
  (when (or (eq status nil)
            (not (or (string= status "enable")
                     (string= status "disable"))))
    (message "Usage: /stegtoy enable or disable"))
  (cond ((string= status "enable")
         (add-hook 'erc-insert-pre-hook #'erc-stegtoy-msg -100))
        ((string= status "disable")
         (remove-hook 'erc-insert-pre-hook #'erc-stegtoy-msg))))

(defun erc-cmd-ENCODE (string)
  (let ((cover (read-string (format "Enter cover text with minimum length %s: " (length string)))))
    (if (>= (length cover) (length string))
        (erc-send-input (erc-stegtoy-encode string cover))
      (message "erc-stegtoy: cover message too short!"))))

(provide 'erc-stegtoy)
;;; erc-stegtoy.el ends here

