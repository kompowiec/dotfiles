(require 'erc)
(require 'erc-fill)

(setq COLORS
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
        ("z" . "\x03\x030,10")))

(setq separators (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                       "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

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
                   "\x03\x030,10"))

(defun erc-encode-msg (string cover-message)
  "Encode the STRING using specified COVER-MESSAGE."
  (let* ((len (length string))
         (chars (split-string string "" t))
         (cover-len (length cover-message))
         (count 0))

    (setq encoded "")
    (while (>= len 1)
      (setq len (- len 1))
      (setq count (+ count 1))
      (let ((cover-char (if (>= count cover-len)
                            "x" ;; Fallback if cover text runs out
                          (substring cover-message (- count 1) count))))
        (setq encoded (concat encoded
                              (cdr (assoc (nth (- count 1) chars) COLORS))
                              cover-char))))
    encoded))

(defun random-ascii-chars (len)
  (setq chars "")
  (while (>= len 1)
    (setq chars
          (concat (char-to-string
                   (+ 97 (random (- 122 97))))
                  chars))
        (setq len (- len 1)))
  chars)

(defun erc-cmd-ENCODE (string)
  (setq encoded (erc-encode-msg string))
  (erc-send-input encoded)
  (setq encoded nil))


(defun erc-decode-msg (message)
  "Decode the message encoded with the specified cover message."
  (let* ((len (length message))
         (string (split-string message (regexp-opt separators) t)))

    (setq decoded "")
    (dolist (chars string decoded)
      (let ((char (car (rassoc chars COLORS)))
            (color-code (cdr (rassoc chars COLORS))))
        (setq decoded
              (push (string-replace color-code char chars) decoded))
        ))
    decoded))

(setq regexp "<.+> \\(.*?\\)\n")

(defun erc-decode-check (string)
  "Extracts the string between <gnufag> and the linefeed character."
  (let ((matched (string-match regexp string))
        (msg (match-string 1 string)))

;;    (setq nicks (match-string "\\<.+\\>" string))
;;    (setq nick (match-string 1 string))
    (setq erc-send-this nil)
    (setq message msg)))


(defun erc-decode-modify ()
  "Decode the message encoded with the specified cover message."
  (when (> (length (split-string message (regexp-opt codes))) 4)
    ;;    (setq message (split-string message (regexp-opt codes)))
    (goto-char (point-max))
    (erc-decode-msg message)
    (erc-concat-decoded-chars decoded)
    (insert decoded-string)))

     
(defun erc-concat-decoded-chars (decoded)
  "kurwa"
  (setq reversed (nreverse (cl-loop for c in decoded collect c)))
  (setq decoded-string (mapconcat 'identity reversed)))



(defun erc-cmd-STEGTOY (status)
  (when (or (eq status nil)
            (not (or (string-equal status "enable")
                     (string-equal status "disable"))))
    (message "Usage: /stegtoy enable or disable"))
  (cond ((string-equal status "enable")
         (add-hook 'erc-insert-pre-hook #'erc-decode-check)
         (add-hook 'erc-insert-modify-hook #'erc-decode-modify))
        ((string-equal status "disable")
         (remove-hook 'erc-insert-pre-hook #'erc-decode-check)
         (remove-hook 'erc-insert-modify-hook #'erc-decode-modify))))


(defun erc-cmd-ENCODE (string)
  "Prompt for cover text and encode STRING."
  (let ((cover-message (read-string "Enter cover text: ")))
    (setq encoded (erc-encode-msg string cover-message))
    (erc-send-input encoded)
    (setq encoded nil)))

