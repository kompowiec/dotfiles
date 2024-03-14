;;; erc-crypt.el --- Symmetric Encryption for ERC

;; Copyright (C) 2011-2020 xristos@sdf.org
;; All rights reserved

;; Modified: 2020-05-10
;; Version: 2.1
;; Author: xristos <xristos@sdf.org>
;; URL: https://github.com/atomontage/erc-crypt
;; Package-Requires: ((cl-lib "0.5"))
;; Keywords: comm

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;   * Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials
;;     provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;
;; Minor mode for ERC that provides PSK encryption.
;;
;; An external `openssl' binary is used for the actual encryption,
;; communicating with Emacs via `call-process-region'.
;;
;;; Usage:
;;
;; Move erc-crypt.el to a directory in your load-path
;;
;; (require 'erc-crypt)
;;
;; M-x erc-crypt-enable  ; Enable encryption for the current ERC buffer
;; M-x erc-crypt-disable ; Disable encryption for the current ERC buffer
;; M-x erc-crypt-set-key ; Set/change key for the current ERC buffer
;;
;;; Features:
;;
;; - Uses external OpenSSL binary for encrypt/decrypt
;; - Visual in-buffer indicator for errors and encrypted messages
;;   sent/received
;; - Auto splits ciphertext in order to get around IRC message limits.
;;   Original formatting is preserved, no information is lost.
;; - Messages are padded to constant size
;;
;;
;;; TODO:
;;
;; + Move to GnuPG for symmetric encryption
;;   (and customizable key derivation from passphrase)
;;
;; + Use OpenSSL for DH key generation
;;
;; + Fully automated authenticated DH key exchange
;;
;;
;;; Notes:
;;
;; erc-crypt should be seen as a proof-of-concept and serve as HOWTO-code
;; in terms of developing similar minor modes for ERC.
;;
;; Do NOT use this if you need STRONG cryptography!

;;; Code:

(require 'erc)
(require 'sha1)
(require 'cl-lib)
(require 'erc-fill)

;; erc-fill doesn't play nice with erc-crypt.el
(defvar-local erc-crypt-fill-function nil)
(make-variable-buffer-local 'erc-fill-function)

(defvar erc-crypt-openssl-path "openssl"
  "Path to openssl binary.")

(defvar erc-crypt-cipher "aes-256-cbc"
  "Cipher to use.  Default is AES CBC.")

(defvar erc-crypt-indicator "☿"
  "String indicator for (in-buffer) encrypted messages.")

(defvar erc-crypt-success-color "PaleGreen"
  "Color to indicate success.")

(defvar erc-crypt-failure-color "#ffff55"
  "Color to indicate failure.")

(defvar erc-crypt-max-length 90
  "Maximum message length.
If input message exceeds it, message is broken up using
`erc-crypt-split-message'.  This is used to work around IRC protocol
message limits.")

(defvar-local erc-crypt-message nil
  "Last message sent (before encryption).")

(defvar-local erc-crypt-key-file nil
  "Path to erc-crypt keyfile.  It's buffer local.")

(defvar-local erc-crypt--left-over nil
  "List that contains message fragments.
Processed by `erc-crypt-post-send' inside `erc-send-completed-hook'.")

(defvar-local erc-crypt--insert-queue nil
  "List that contains message fragments, before insertion.
Managed by `erc-crypt-maybe-insert'.")

(defvar-local erc-crypt--post-insert nil
  "Contains received public keys.
Type of keys: `x25519_pub', `ed25519_pub', and `ed25519_sig' signature.
Saving happens in `erc-post-insert-hook' in function
`erc-crypt-dh-secret' after positive verifying with
`erc-crypt-verify-sig'")

(defvar erc-crypt-prefix "LVX"
  "String prefixed to all encrypted messages sent/received.")

(defvar erc-crypt-postfix "IAO"
  "String postfixed to all encrypted messages sent/received.")

(defvar erc-crypt-dh-prefix "DHS"
  "String prefixed to x25519 pubkey.")
  
(defvar erc-crypt-dh-postfix "DHE"
  "String postfixed to x25519 pubkey.")

(defvar erc-crypt-ed-pubkey-prefix "CLX"
  "String prefixed to ed25519 pubkey.  It's needed for verify x25519 pubkey.")

(defvar erc-crypt-ed-pubkey-postfix "LXC"
  "String prefixed to ed25519 pubkey.  It's needed for verify x25519 pubkey.")

(defvar erc-crypt-ed-sig-prefix "SIG"
  "String prefixed to ed25519 signature.
It's needed for verify x25519 pubkey.")

(defvar erc-crypt-ed-sig-postfix "GTS"
  "String postfixed to ed25519 signature.
It's needed for verify x25519 pubkey.")

(defvar-local erc-crypt-msg-type ""
"Type of received message.  It's can be:
`x25519_pub' - needed for encryption
`ed25519_pub' - public key needed for verify `x25519_pub'
`ed25519_sig' - signature needed for verify `x25519_pub'
`normal-encrypted' - encrypted message
`plain-text'       - just plain text.
Must be string.")

(defvar erc-crypt-dir-userdef "~/.emacs.d/irc/erc-crypt"
  "User defined directory for erc-crypt.")

(defvar erc-crypt--dir (expand-file-name
                        (file-name-as-directory erc-crypt-dir-userdef))
  "Erc crypt directory contained public and private keys.")


(define-global-minor-mode erc-crypt-on-off erc-crypt-mode
  erc-crypt-find-key) ;; enable if key found

(add-hook 'erc-insert-pre-hook #'erc-crypt-on-off-check)
 ;; enable if '----CRYPT ON----' string found

(define-minor-mode erc-crypt-mode
  "Per buffer encryption for ERC."
  nil " CRYPT" nil
  (if erc-crypt-mode
      ;; Enabled
      (progn
        (add-hook 'erc-send-pre-hook        #'erc-crypt-maybe-send nil t)
        (add-hook 'erc-send-modify-hook     #'erc-crypt-maybe-send-fixup nil t)
        (add-hook 'erc-send-completed-hook  #'erc-crypt-post-send nil t)
        (add-hook 'erc-insert-pre-hook      #'erc-crypt-pre-insert nil t)
        (add-hook 'erc-insert-modify-hook   #'erc-crypt-maybe-insert nil t)
        (add-hook 'erc-insert-post-hook     #'erc-crypt-dh-save nil t)
        ;; Reset buffer locals
        (setq erc-crypt--left-over    nil
              erc-crypt--insert-queue nil
              erc-crypt-fill-function erc-fill-function
              erc-fill-function       nil))

    ;; Disabled
    (progn
      (remove-hook 'erc-send-pre-hook       #'erc-crypt-maybe-send t)
      (remove-hook 'erc-send-modify-hook    #'erc-crypt-maybe-send-fixup t)
      (remove-hook 'erc-send-completed-hook #'erc-crypt-post-send t)
      (remove-hook 'erc-insert-pre-hook     #'erc-crypt-pre-insert t)
      (remove-hook 'erc-insert-modify-hook  #'erc-crypt-maybe-insert t)
      (remove-hook 'erc-insert-post-hook    #'erc-crypt-dh-save t)
      (setq erc-fill-function erc-crypt-fill-function
            erc-crypt-fill-function nil))))



;;;
;;; Internals
;;;



(defun erc-crypt--message (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS."
  (let ((message-truncate-lines t))
    (message "erc-crypt: %s" (apply #'format format-string args))))

(defun erc-crypt-prefix-check ()
  "Set prefix of received message based on `erc-crypt-msg-type' variable.
The variable is set in `erc-crypt-string-check' function always when
 received message."
  (let ((prefix (cond ((string= erc-crypt-msg-type "normal-encrypted")
                erc-crypt-prefix)
               ((string= erc-crypt-msg-type "x25519_pub")
                erc-crypt-dh-prefix)
               ((string= erc-crypt-msg-type "ed25519_pub")
                erc-crypt-ed-pubkey-prefix)
               ((string= erc-crypt-msg-type "ed25519_sig")
                erc-crypt-ed-sig-prefix))))
    prefix))

(defun erc-crypt-postfix-check ()
  (let ((postfix (cond ((string= erc-crypt-msg-type "normal-encrypted")
                        erc-crypt-postfix)
                       ((string= erc-crypt-msg-type "x25519_pub")
                        erc-crypt-dh-postfix)
                       ((string= erc-crypt-msg-type "ed25519_pub")
                        erc-crypt-ed-pubkey-postfix)
                       ((string= erc-crypt-msg-type "ed25519_sig")
                        erc-crypt-ed-sig-postfix))))
    postfix))


(cl-defmacro erc-crypt--with-message ((message) &rest body)
  "Conveniently work with narrowed region as implemented by ERC hooks.

Search for and extract an encrypted message (if present),
then bind MESSAGE to it, delete the encrypted string from buffer
and execute BODY. Finally, restore ERC text properties.

See `erc-send-modify-hook' and `erc-insert-modify-hook'."
  (declare (indent defun))
  (let ((start (cl-gensym)))
    `(when erc-crypt-mode
       (goto-char (point-min))
       (let* ((prefix (erc-crypt-prefix-check))
              (postfix (erc-crypt-postfix-check))
              (,start nil))
         (when (re-search-forward (concat prefix ".+" postfix) nil t)
           (let ((,message (buffer-substring (+ (match-beginning 0)
                                                (length prefix))
                                             (- (match-end 0)
                                                (length postfix))))
                 (,start (match-beginning 0)))
             (delete-region (match-beginning 0) (match-end 0))
             (goto-char ,start)
             ,@body)
           (erc-restore-text-properties))))))

(defun erc-crypt--time-millis ()
  "Return current time (time since Unix epoch) in milliseconds."
  (cl-destructuring-bind (sec-h sec-l micro &optional _) (current-time)
    (+ (* (+ (* sec-h (expt 2 16))
             sec-l)
          1000)
       (/ micro 1000))))

(defun erc-crypt--generate-iv ()
  "Generate a suitable IV to be used for message encryption.
Return IV as a 128bit hex string."
  (substring (sha1 (mapconcat
                    #'int-to-string
                    (list (erc-crypt--time-millis)
                          (random t)
                          (random t))
                    ""))
             0 32))

(defun erc-crypt--pad (list)
  "Pad message or fragments in LIST to `erc-crypt-max-length' bytes.
Return a list of padded message or list of fragments.

Resulting messages are of the form MMMMMMMMXXXPS.
                                   <-max len->
MMM are original message bytes.
XXX are bytes used for padding.
P is a single byte that is equal to the number of X (padding bytes)
S is a single byte that is equal to 1 when the message is a fragment,
0 if not or if final fragment."
  (cl-labels ((do-pad (string split-tag)
                (let* ((len  (length string))
                       (diff (- erc-crypt-max-length len))
                       (pad  (cl-loop repeat diff concat
                                      (string (random 256)))))
                  (concat string pad (string split-tag) "@" (string len)))))
    (cl-loop for (msg . rest) on list
             if rest collect (do-pad msg 1)
             else collect    (do-pad msg 0))))


(defun erc-crypt--split (string)
  "Split STRING into substrings that are at most `erc-crypt-max-length' long.
Splitting does not take into account word boundaries or whitespace.

Return list of substrings."
  (cl-loop with len   = (length string)
           with start = 0
           with max   = erc-crypt-max-length
           while (< start len)
           collect (substring string start (min len (cl-incf start max)))))

;;;
;;; Public API
;;;


(cl-defun erc-crypt-encrypt (string)
  "Encrypt STRING with `erc-crypt-key'.
An IV generated dynamically by `erc-crypt--generate-iv' is used for encryption.

If `erc-crypt-key' is nil, ask for a key interactively.

Return BASE64 encoded concatenation of IV and CIPHERTEXT which should be
BASE64 encoded as well. Return nil on all errors."
  (unless erc-crypt-key-file
    (erc-crypt--message "Encryption key not found"))
  (condition-case ex
      (let ((iv  (erc-crypt--generate-iv))
            (key erc-crypt-key-file))
        (cl-multiple-value-bind (status result)
            (with-temp-buffer
              (insert (encode-coding-string string 'utf-8))
              (list (call-process-region
                     (point-min) (point-max)
                     erc-crypt-openssl-path t '(t nil) nil
                     "enc" "-a" (concat "-" erc-crypt-cipher)
                     "-iv" iv "-kfile"
                     key "-salt")
                    (buffer-string)))
          (unless (= status 0)
            (erc-crypt--message "Output: %s" result)
            (erc-crypt--message
             "Non-zero return code %s from openssl (encrypt)" status)
            (cl-return-from erc-crypt-encrypt nil))
          (base64-encode-string (concat iv result) t)))
    ('error
     (erc-crypt--message "%s (process error/erc-crypt-encrypt)"
                         (error-message-string ex))
     nil)))



(cl-defun erc-crypt-decrypt (string)
  "Decrypt STRING with keyfile found at `erc-crypt-key-file'.
the IV as a 16 byte hex string
and the CIPHERTEXT, which must be BASE64 encoded as well."
  (unless erc-crypt-key-file
    (erc-crypt--message "No key set, could not decrypt")
    (cl-return-from erc-crypt-decrypt nil))
  (condition-case ex
      (let* ((str (base64-decode-string string))
             (iv  (substring str 0 32))
             (ciphertext (substring str 32))
             (key erc-crypt-key-file)) ; because 'erc-crypt-key-file' is buffer
        (cl-multiple-value-bind (status result) ; local then after buffer is
            (with-temp-buffer                   ; switch to temp buffer
              (insert ciphertext)               ; 'erc-crypt-key-file' variable
              (list (call-process-region        ; is not available. Then it
                     (point-min) (point-max)    ; must be passed as local
                     erc-crypt-openssl-path t '(t nil) nil     ; variable
                     "enc" "-d" "-a" (concat "-" erc-crypt-cipher)
                     "-iv" iv "-kfile"
                     key "-salt")
                    (buffer-string)))
          (unless (= status 0)
            (erc-crypt--message
             "Non-zero return code %s from openssl (erc-crypt-decrypt)" status)
            (cl-return-from erc-crypt-decrypt nil))
          result))
    ('error
     (erc-crypt--message "%s (process error/erc-crypt-decrypt)"
                         (error-message-string ex))
     nil)))

(defun erc-crypt-maybe-send (string)
  "Encrypt STRING and send to receiver.  Run as a hook in `erc-send-pre-hook'.
STRING should contain user input.  In order to get around IRC protocol
message size limits, STRING is split into fragments and padded to a
constant size, `erc-crypt-max-length', by calling `erc-crypt-split-message'.
The resulting padded fragments are encrypted and sent separately,
the original message reconstructed at the receiver end, with the original
formatting preserved intact.

On errors, do not send STRING to the server."
  (when (and erc-crypt-mode
             ;; Skip ERC commands
             (not (string= "/" (substring string 0 1))))
    (let* ((encoded   (encode-coding-string string 'utf-8 t))
           (split     (erc-crypt-split-message encoded))
           (encrypted (mapcar #'erc-crypt-encrypt split)))
      (cond ((cl-some #'null encrypted)
             (erc-crypt--message "Message will not be sent")
             (setq erc-send-this nil))
            (t
             ;; str is dynamically bound
             (defvar str nil)
             (setq erc-crypt-message str
                   str (concat erc-crypt-prefix
                               (cl-first encrypted)
                               erc-crypt-postfix)
                   erc-crypt--left-over
                   (cl-rest encrypted)))))))


(defun erc-crypt-find-key ()
  "Find key file and read it."
 (when (and (derived-mode-p  'erc-mode)
             (erc-default-target))
    (let* ((channel   (erc-default-target))
           (target    (when   (string= (substring channel 0 1) "#") t))
           (key-path  (format "%s/servers/%s/%s/%s/%s"
                              erc-crypt--dir ;dir tree of erc-crypt - 1st %s
                              (erc-format-network) ;connected server - 2nd %s
                              (if (eq target t);<--if channel search in
                                  "channels";<- 'channels' dir, else search in
                                "ircers"); personal 'ircers' directory - 3rd %s
                              channel;<--- channel or friend directory - 4rd %s
                              channel));<- channel or friend content
           (key-exists (file-exists-p key-path)));<- if key found
      (when key-exists (progn (erc-crypt-enable);<-- then enable erc-crypt
                              (setq erc-crypt-key-file key-path))))));<- set
;;                                                               path of key


(defun erc-crypt-on-off-check (string)
  "Enable erc-crypt if '----CRYPT ON----' string found in STRING.
Needed for receiving public keys and signature."
  (unless erc-crypt-mode
    (when (eq major-mode 'erc-mode)
      (when (string-match "----CRYPT ON----" string)
        (erc-crypt-enable)))))


(defun erc-crypt-maybe-send-fixup ()
  "Restore encrypted message back to its plaintext form.
This happens inside `erc-send-modify-hook'."
  (erc-crypt--with-message (msg)
    (insert erc-crypt-message)
    (goto-char (point-min))
    (insert (concat (propertize erc-crypt-indicator 'face
                                (list :foreground erc-crypt-success-color))
                    " "))))


(cl-defun erc-crypt-string-check (string)
  "Check type of irc message in STRING for erc-crypt."
  (cond ((string-match (concat erc-crypt-dh-prefix "\\(.+\\)"
                               erc-crypt-dh-postfix)
                       string) ;; if string is a public key
         (setq erc-crypt-msg-type "x25519_pub")
         (erc-crypt-dh-receiver string))

        ((string-match (concat erc-crypt-ed-pubkey-prefix "\\(.+\\)"
                               erc-crypt-ed-pubkey-postfix)
                       string)  ;; if string is public key for signature
         (setq erc-crypt-msg-type "ed25519_pub")
         (erc-crypt-dh-receiver string))
        
        ((string-match (concat erc-crypt-ed-sig-prefix "\\(.+\\)"
                               erc-crypt-ed-sig-postfix)
                       string) ;; if string is a sign of public key
         (setq erc-crypt-msg-type "ed25519_sig")
         (erc-crypt-dh-receiver string))

        ((string-match (concat erc-crypt-prefix "\\(.+\\)"
                               erc-crypt-postfix)
                       string) ;; if string is normal encrypted string
         (setq erc-crypt-msg-type "normal-encrypted"))

        (t
         (setq erc-crypt-msg-type "plain-text"))));<-- if none above


(defun erc-crypt-dh-receiver (string)
  "Decode dh key from STRING and later reconstruct fragments if its in parts."
  (let* ((strb64 (match-string 1 string)) ;; get matched string
         (dec64  (base64-decode-string strb64)) ;; decode string from base64
         (len    (length dec64)) ;; needed for remove split tag
         (split  (aref dec64 (- len 1))) 
         (final  (substring dec64 0 (- len 1))))
    (push (cons  final split) erc-crypt--insert-queue)
    (when        (= split 1) (setq erc-insert-this nil))))


(defun erc-crypt-pre-insert (string)
  "Decrypt STRING and insert it into `erc-crypt--insert-queue'.
If decrypted message is a fragment, `erc-insert-this' is set to nil.
Does not display message and does not trigger `erc-insert-modify-hook'."
  (erc-crypt-string-check string)
  (when (string=  erc-crypt-msg-type "normal-encrypted")
         (let*   ((msg       (match-string 1 string))
                  (decrypted (erc-crypt-decrypt msg)))
           (if     decrypted
               (let* ((len       (length decrypted))
                      (split     (aref decrypted (- len 3)));<- python
                      (original  (aref decrypted (- len 1)));<- compatible
                      (decrypted (substring decrypted 0 original)))
                 (push (cons decrypted split) erc-crypt--insert-queue)
                 (if (= split 1) (setq erc-insert-this nil)))
             ;; Error, erc-insert-this will be set to t so it's not possible
             ;; for multiple error-indicating conses to be inserted in the
             ;; queue.
             (push (cons :error nil) erc-crypt--insert-queue)))))


(defun erc-crypt--insert (msg &optional error)
  (insert (concat (if error "(decrypt error) " "")
                  (decode-coding-string msg 'utf-8 :nocopy)))
  (goto-char (point-min))
  (insert (concat
           (propertize
            erc-crypt-indicator 'face
            (list :foreground
                  (if error
                      erc-crypt-failure-color erc-crypt-success-color)))
           " "))
  (setq erc-crypt--insert-queue nil))


(defun erc-crypt-maybe-insert ()
  "Display decrypted messages and do fragment reconstruction.
This happens inside `erc-insert-modify-hook'."
  (erc-crypt--with-message (msg)
    (cl-loop with first = (cl-first erc-crypt--insert-queue)
             with rest  = (cl-rest erc-crypt--insert-queue)
             with msg   = (car first)
             with tag   = (cdr first)
             ;; Incomplete message fragment
             when (equal tag 1)
             do (cl-return)
             ;; Complete message in one fragment
             when (and (equal tag 0)
                       (null rest))
             do (erc-crypt--insert msg)
             (setq erc-crypt--post-insert msg)
             (cl-return)
             ;; Either an error or final fragment
             for fragment in rest collect (car fragment) into out
             finally
             (let ((out (mapconcat #'identity (nreverse out) "")))
               (if (eql msg :error)
                   (erc-crypt--insert out t)
                 (setq erc-crypt--post-insert (concat out msg))
                 (erc-crypt--insert (concat out msg)))))))

(defun erc-crypt-dir-check (dir)
  "Check if DIR exists and if not make it."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun erc-crypt-sig-b64-convert (tempdir nick)
  (let ((sig (concat tempdir nick)))
    (call-process
     "base64" nil ;; no infile
     `(:file ,(concat sig "-ed25519_sig.bin"))
     nil "-d" (concat sig "-ed25519_sig.b64"))
    (delete-file (concat sig "-ed25519_sig.b64")))) 

(defun erc-crypt-dh-pubkey-check (tempdir nick)
  "Check if all needed keys in TEMPDIR with NICK in filename exists."
  (when (and (file-exists-p (concat tempdir nick "-ed25519_sig.bin"))
             (file-exists-p (concat tempdir nick "-ed25519_pub.pem"))
             (file-exists-p (concat tempdir nick "-x25519_pub.pem")))
    t))

(defun erc-crypt-dh-save ()
  "Save received public x25519 key."
  (when (or (string=  erc-crypt-msg-type "x25519_pub")
            (string=  erc-crypt-msg-type "ed25519_sig")
            (string=  erc-crypt-msg-type "ed25519_pub"))
    (let* ((key-or-sig  erc-crypt--post-insert);<---- received key or signature
           (nick        (if (string= (substring (erc-default-target) 0 1) "#")
                            (erc-crypt-get-last-message-nick);<-if keys found
                          (erc-default-target)));^ on channel then get nick
           ;;                     ^ from re-search-backward, else get nick from
           ;;                                                erc-default-target
           (dir         (format
                         "%sservers/%s/ircers/%s/" ;; <---- directory tree with
                         erc-crypt--dir ;  <---- pubkeys
                         (erc-format-network) ;; directory of connected server
                         nick)) ;; dir of current channel or friend if /query 
           (keyfile      (concat (file-name-as-directory dir) nick));<-partial
           (tempdir      (concat erc-crypt--dir "temp/" nick "/")));  |filename
      ;;                   temporary dir for not yet verified keys    |and path
      (erc-crypt-dir-check dir)                                    ;  |for save
      (erc-crypt-dir-check tempdir)                                ;  |keys
      
      ;;   because ed25519_sig is binary file it must be saved to text file and 
      (if (string= erc-crypt-msg-type "ed25519_sig");then converted from base64
          (progn (with-temp-file                    ;by separate process
                     (concat tempdir nick "-ed25519_sig.b64")
                   (insert key-or-sig))
                 (erc-crypt-sig-b64-convert tempdir nick))
        (with-temp-file (concat tempdir nick "-" erc-crypt-msg-type ".pem")
          (insert key-or-sig)))

      (when (erc-crypt-dh-pubkey-check tempdir nick)
        (let ((status (erc-crypt-verify-sig tempdir nick)))
          (if (= status 0) ;; save new keys only if signature verified positive
              (progn
                (erc-crypt--message
                 "Received x25519 and ed25519 keys.\
 Signature verified successfully.")
                (erc-crypt-dir-check (concat dir "/archive"))
                (when (directory-files dir)       ; if old keys found move them
                  (erc-crypt-move-keys            ; to archive - currently only 
                   dir (concat dir "/archive/"))) ; one keypairs are archived
                ;;                                      make shared secret file
                (erc-crypt-dh-secret tempdir keyfile nick);<---for encryption
                (erc-crypt-move-keys
                 tempdir dir)

                (set-buffer  ;; buffer with received keys is unneeded now
                 (car (erc-buffer-list-with-nick
                       nick
                       (get-buffer-process (erc-format-network)))))
                (kill-buffer-and-window)

                (erc-crypt-find-key)) ;; <- when everything is OK then find new
            (erc-crypt--message       ;;    key
             "Received x25519 and ed25519 keys with signature,\
 but verify failed."))))))

    (setq erc-crypt--post-insert nil
          erc-crypt-msg-type     nil))

(defun erc-crypt-move-keys (dir1 dir2)
  "Move public keys located in DIR1 to DIR2 directory."
  (cl-loop for keyfile in (directory-files dir1) do
           (unless (or (string= keyfile ".")  ;; omit currenct dir
                       (string= keyfile "..") ;; omit parent dir
                       (string= keyfile "archive"));  omit archive dir
             (copy-file (concat dir1 "/" keyfile) dir2 t) ;copy content
             (delete-file (concat dir1 "/" keyfile))))) ; of dir passed as 
;;           remove content of tempdir                           as option

(defun erc-crypt-verify-sig (tempdir nick)
  "Verify received ed25519 signature, then check verified status in TEMPDIR.
If positive copy to DIR / NICK directory and
delete after original ~/.emasc.d/irc/erc-crypt/temp/verify-status after all."
  (let* ((keyname (concat tempdir "/" nick))
         (status  (call-process
                 erc-crypt-openssl-path
                 nil ;; no infile
                 nil ;; no output
                 nil
                 "pkeyutl" "-verify" "-pubin" "-inkey"
                 (concat keyname "-ed25519_pub.pem")
                 "-rawin" "-in" (concat keyname "-x25519_pub.pem")
                 "-sigfile" (concat keyname "-ed25519_sig.bin"))))
    status))

(defun erc-crypt-dh-secret (tempdir keyfile nick)
  "Make shared secret for encrytpion in TEMPDIR.
Then move to KEYFILE with filename NICK."
  (call-process erc-crypt-openssl-path
                nil ;; no infile
                `(:file ,(concat keyfile)) ;; output
                nil "pkeyutl" "-derive" "-inkey"
                (concat erc-crypt--dir "secret/x25519_priv.pem")
                "--peerkey" (concat tempdir nick "-x25519_pub.pem")
                "-out" keyfile))

(defun erc-crypt-get-last-message-nick ()
  "Get the nickname of the last message in the ERC chat buffer."
  (interactive)
  (goto-char (point-max))
  (let* ((nick-position  (re-search-backward "<.+>" nil t nil));<--- search for
         (beg            (progn (goto-char nick-position) ;                nick
                                (point))) ; return point of nick
         (end            (search-forward ">"))) ;; end of nick position
    (when nick-position
      (let* ((nick-line  (buffer-substring-no-properties beg end))
             (nick-match (string-match "<.+>" nick-line)) ;; nick found
             (nick       (match-string 0 nick-line))) ;; bind nick to variable
        (goto-char       (point-max))
        nick))))
;; return nick

(defun erc-crypt-post-send (string)
  "Send message fragments placed in `erc-crypt--left-over' to remote end.
STRING is unused, but required."
  (unwind-protect
      (cl-loop for m in erc-crypt--left-over do
            (erc-message "PRIVMSG"
                         (concat (erc-default-target) " "
                                 (concat erc-crypt-prefix m erc-crypt-postfix))
                         ))
    (setq erc-crypt--left-over nil)))


(defun erc-crypt-split-message (string)
  "Split STRING and pad to maximum size if needed."
  (let* ((len (length string)))
    (cond ((<= len erc-crypt-max-length)
           ;; Pad to maximum size if needed
           (erc-crypt--pad (list string)))
          (t
           (erc-crypt--pad (erc-crypt--split string))))))


(defun erc-crypt--dh-split (string)
  "Split key or signature in STRING."
  (cl-loop with len   = (length string)
           with start = 0
           with max   = 150
           while (< start len)
           collect (substring string start (min len (cl-incf start max)))))

(defun erc-crypt--dh-tag (list)
  "Tag fragments of key in LIST.
Without padding because it is unneeded when sending keys."
  (cl-labels ((do-tag (string dh-tag)
                (concat string (string dh-tag))))
    (cl-loop for (msg . rest) on list
             if rest collect (do-tag msg 1)
             else collect    (do-tag msg 0))))

(defun erc-crypt-split-dh (string)
  "Firstly split key in STRING, then tag parts."
  (erc-crypt--dh-tag (erc-crypt--dh-split string)))

(defun erc-crypt-dh-pubkey-read ()
  "Read x25519_pub.pem key found in `erc-crypt--dir'/secret/ directory.
It is used for make shared secret file for encrypting messages."
    (with-temp-buffer
      (insert-file-contents (concat erc-crypt--dir "secret/x25519_pub.pem"))
      (let* ((dh-split (erc-crypt-split-dh (buffer-string)))
             (dh-b64   (mapcar #'(lambda (dh-split)
                                 (base64-encode-string dh-split t))
                             dh-split)))
    dh-b64)))

(defun erc-crypt-dh-ed-pubkey-read ()
  "Read ed25519_pub.pem key found in `erc-crypt--dir'/secret/ directory.
It is used for verify x25519_pub.pem key."
    (with-temp-buffer
    (insert-file-contents (concat
                           erc-crypt--dir "secret/ed25519_pub.pem"))
    (let* ((ed-split (erc-crypt-split-dh (buffer-string)))
           (ed-b64   (mapcar #'(lambda (ed-split)
                               (base64-encode-string ed-split t))
                           ed-split)))
    ed-b64)))

(defun erc-crypt-dh-ed-sig-read ()
  "Read ed25519_sig.bin signature file found in `erc-crypt--dir'/secret/ dir.
It is used for verify x25519_pub.pem key."
    (with-temp-buffer
    (insert-file-contents (concat
                           erc-crypt--dir "secret/ed25519_sig.bin"))
    (let* ((sig-b64   (base64-encode-string (buffer-string)))
           (sig-split (erc-crypt-split-dh sig-b64))
           (sig-b64   (mapcar #'(lambda (sig-split)
                                (base64-encode-string sig-split t))
                            sig-split)))
    sig-b64)))

(defun erc-crypt-dh-ex (nick)
  "Read and public keys and signature and send to NICK."
  (let ((dh-b64  (erc-crypt-dh-pubkey-read))
        (ed-b64  (erc-crypt-dh-ed-pubkey-read))
        (sig-b64 (erc-crypt-dh-ed-sig-read)))


  (erc-message "PRIVMSG" (concat (car nick) " " "----" "CRYPT ON" "----"))
  
  (erc-crypt-send-signed-pub (car nick)
                             erc-crypt-dh-prefix
                             dh-b64
                             erc-crypt-dh-postfix)
  
  (erc-crypt-send-signed-pub (car nick)
                             erc-crypt-ed-pubkey-prefix
                             ed-b64
                             erc-crypt-ed-pubkey-postfix)
  
  (erc-crypt-send-signed-pub (car nick)
                             erc-crypt-ed-sig-prefix
                             sig-b64
                             erc-crypt-ed-sig-postfix)))

                               
(defun erc-crypt-send-signed-pub (nick prefix-type signed_pub postfix-type)
  "Send keys to NICK with postfix and prefix suggestive type of message.
PREFIX-TYPE like POSTFIX-TYPE suggest type of message, and SIGNED_PUB is signed
x25519 public key."
  (unwind-protect
      (cl-loop for dh-part in signed_pub do
               (erc-message "PRIVMSG" (concat nick " "
                                              prefix-type dh-part
                                              postfix-type )))))
  
(defun erc-crypt-dh-multi-ex (nicks)
  "Send public keys to NICKS."
  (cl-loop for nick in nicks do
           (erc-crypt-dh-ex nick)))

(defun erc-crypt-dir-clean (dir)
  "Remove all keys in DIR."
  (cl-loop for file in (directory-files dir) do
           (unless (or (string= file ".")
                       (string= file ".."))
           (delete-file (concat dir "/" file)))))

(defun erc-crypt-dh-generate-keys ()
  "Generate own public and private x25519 keys."
  (erc-crypt-dir-check "~/.emacs.d/irc/erc-crypt/secret")
  (when (directory-files "~/.emacs.d/irc/erc-crypt/secret/")
    (erc-crypt-dir-clean "~/.emacs.d/irc/erc-crypt/secret/"))

  (let* ((secret-dir (concat (concat erc-crypt--dir "secret")))  ;  destination
         (status-priv (call-process erc-crypt-openssl-path nil;no infile      |
                              `(:file ,(concat secret-dir "x25519_priv.pem"));^
                              nil ;; don't display result
                              "genpkey" "-algorithm" "x25519" "-out"
                              (concat secret-dir "/x25519_priv.pem")))

         (status-pub (call-process
                      erc-crypt-openssl-path nil ; no infile   |    destination 
                      `(:file ,(concat secret-dir "/x25519_pub.pem"));<----^
                      nil ;; don't display result
                      "pkey" "-in" (concat secret-dir "/x25519_priv.pem")
                      "-pubout" "-out" (concat secret-dir "/x25519_pub.pem"))))
    (if (and (= status-priv 0) 
             (= status-pub  0)) ;; if everything OK
        (erc-crypt-generate-ed-sig-keys secret-dir)
      (cond ((not (= status-priv 0))
             (erc-crypt--message (concat "Generate " secret-dir
                                         "x25519_priv.pem key failed.")))
            ((not (= status-pub 0))
             (erc-crypt--message (concat "Generate " secret-dir
                                         "x25519_pub.pem key failed.")))))))
    
(defun erc-crypt-generate-ed-sig-keys (secret-dir)
  "Generate public and private signing ed25519 keys in SECRET-DIR."
  (let ((ed-status-priv (call-process
                      erc-crypt-openssl-path
                      nil `(:file ,(concat secret-dir "/ed25519_priv.pem"))
                      nil "genpkey" "-algorithm" "Ed25519" "-out"
                      (concat secret-dir "/ed25519_priv.pem")))

        (ed-status-pub (call-process
                     erc-crypt-openssl-path
                     nil `(:file ,(concat secret-dir "/ed25519_pub.pem"))
                     nil "pkey" "-in" (concat secret-dir "/ed25519_priv.pem")
                     "-pubout" "-out" (concat secret-dir "/ed25519_pub.pem"))))
    (if (and (= ed-status-priv 0) 
             (= ed-status-pub  0)) ;; if everything OK
        (erc-crypt-ed-keysign secret-dir)
      (cond ((not (= ed-status-priv 0))
             (erc-crypt--message (concat "Generate " secret-dir
                                         "ed25519_priv.pem key failed.")))
            ((not (= ed-status-pub 0))
             (erc-crypt--message (concat "Generate " secret-dir
                                         "ed25519_pub.pem key failed."))))))
  (erc-crypt-ed-keysign secret-dir))

(defun erc-crypt-ed-keysign (secret-dir)
  "Sign keys found in SECRET-DIR."
  (let ((ed-status-sig (call-process
                     erc-crypt-openssl-path
                     nil `(:file ,(concat secret-dir "/ed25519_sig.bin"))
                     nil "pkeyutl" "-sign" "-inkey"
                     (concat secret-dir "/ed25519_priv.pem")
                     "-out" (concat secret-dir "/ed25519_sig.bin")
                     "-rawin" "-in" (concat secret-dir "/x25519_pub.pem"))))
    (if (= ed-status-sig 0)
        (erc-crypt--message "Keypairs generated and signed succesfully.")
      (erc-crypt--message "Generate keypairs or signing failed."))))


(defun erc-cmd-CRYPT (option &optional &rest args)
  (let ((enabled (when erc-crypt-mode t)))
    (cond ((string= option "genkeys")
           (erc-crypt-dh-generate-keys))
          ;;
          ;; send public keys to all
          ;; on channel in privmsg  
          ((string= option "dh-all")
           (erc-crypt-dh-multi-ex (erc-get-channel-nickname-list)))
          ;;
          ;; send public key to nick
          ((string= option "dh")
           (cond ((> (length args) 1)
                  (erc-crypt-dh-multi-ex args))
                 ((= 1 (length args))
                  (erc-crypt-dh-ex args))
                 ((eq  args nil)
                  (erc-crypt--message "nick or nicks required required"))))
          ;;
          ;; enable erc-crypt
          ((string= option "on")
           (erc-crypt-enable))
          ;;
          ;; disable erc-crypt
          ((string= option "off")
           (erc-crypt-disable))
          
          ;; send clear text message
          ((string= option "ct")
           (erc-crypt-disable)
           (let ((len (length (format "%s" args))))
             (erc-send-input (substring (format "%s" args) 1 (- len 1))))
           (erc-crypt-enable)))))

;;;
;;; Interactive
;;;


;;;###autoload
(defun erc-crypt-enable ()
  "Enable PSK encryption for the current buffer."
  (interactive)
  (when (eq major-mode 'erc-mode) t)
  (erc-crypt-mode t))

;;;###autoload
(defun erc-crypt-disable ()
  "Disable PSK encryption for the current buffer."
  (interactive)
  (when (eq major-mode 'erc-mode) t)
  (erc-crypt-mode -1))

(provide 'erc-crypt)
;;; erc-crypt.el ends here
