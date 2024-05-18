;;; irc-e.el --- fork of erc-crypt.el

;;; Original Author:
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

;;; Usage:
;;
;; Move irc-e.el to a directory in your load-path
;;
;; (require 'irc-e)
;; (setq irc-e-dir-userdef "~/path/to/your/dir")
;; (irc-e-on-off 1)
;; 
;;

;;; Code:

(require 'erc)
(require 'cl-lib)
(require 'erc-fill)
(require 'erc-track)

;; erc-fill doesn't play nice with irc-e.el
(defvar-local irc-e-fill-function nil)
(make-variable-buffer-local 'erc-fill-function)

(defvar irc-e-openssl-path "openssl"
  "Path to openssl binary.")

(defvar irc-e-cipher "aes-256-cbc"
  "Cipher to use.  Default is AES CBC.")

(defvar irc-e-indicator "☿"
  "String indicator for (in-buffer) encrypted messages.")

(defvar irc-e-success-color "PaleGreen"
  "Color to indicate success.")

(defvar irc-e-failure-color "#ffff55"
  "Color to indicate failure.")

(defvar irc-e-max-length 90
  "Maximum message length.
If input message exceeds it, message is broken up using
`irc-e-split-message'.  This is used to work around IRC protocol
message limits.")

(defvar-local irc-e-message nil
  "Last message sent (before encryption).")

(defvar-local irc-e-key-file nil
  "Path to irc-e keyfile.  It's buffer local.")

(defvar-local irc-e--left-over nil
  "List that contains message fragments.
Processed by `irc-e-post-send' inside `erc-send-completed-hook'.")

(defvar-local irc-e--insert-queue nil
  "List that contains message fragments, before insertion.
Managed by `irc-e-maybe-insert'.")

(defvar-local irc-e--post-insert nil
  "Contains received public keys.
Type of keys: `x25519_pub', `ed25519_pub', and `ed25519_sig' signature.
Saving happens in `erc-post-insert-hook' in function
`irc-e-dh-secret' after positive verifying with
`irc-e-verify-sig'")

(defvar irc-e-prefix "LVX"
  "String prefixed to all encrypted messages sent/received.")

(defvar irc-e-postfix "IAO"
  "String postfixed to all encrypted messages sent/received.")

(defvar irc-e-dh-prefix "DHS"
  "String prefixed to x25519 pubkey.")
  
(defvar irc-e-dh-postfix "DHE"
  "String postfixed to x25519 pubkey.")

(defvar irc-e-ed-pubkey-prefix "CLX"
  "String prefixed to ed25519 pubkey.  It's needed for verify x25519 pubkey.")

(defvar irc-e-ed-pubkey-postfix "LXC"
  "String prefixed to ed25519 pubkey.  It's needed for verify x25519 pubkey.")

(defvar irc-e-ed-sig-prefix "SIG"
  "String prefixed to ed25519 signature.
It's needed for verify x25519 pubkey.")

(defvar irc-e-ed-sig-postfix "GTS"
  "String postfixed to ed25519 signature.
It's needed for verify x25519 pubkey.")

(defvar-local irc-e-msg-type nil
"Type of received message.  It's can be:
`x25519_pub' - needed for encryption
`ed25519_pub' - public key needed for verify `x25519_pub'
`ed25519_sig' - signature needed for verify `x25519_pub'
`normal-encrypted' - encrypted message
`plain-text'       - just plain text.
Must be string.")

(defvar irc-e-dir-userdef "~/.emacs.d/irc/irc-e"
  "User defined directory for irc-e.")

(defvar irc-e-visible nil
  "t if erc in current buffer and irc-e enabled.")

(defvar irc-e--dir (expand-file-name
                        (file-name-as-directory irc-e-dir-userdef))
  "Erc crypt directory contained public and private keys.")

(defvar irc-e-external-buf nil
  "Buffers other than erc.")

(define-global-minor-mode irc-e-on-off irc-e-mode
  irc-e-find-key) ;; enable if key found

(add-hook 'erc-insert-pre-hook #'irc-e-on-off-check)
 ;; enable if '----CRYPT ON----' string found

(define-minor-mode irc-e-mode
  "Per buffer encryption for ERC."
  nil " CRYPT" nil
  (if irc-e-mode
      ;; Enabled
      (progn
        (add-hook 'erc-send-pre-hook        #'irc-e-maybe-send nil t)
        (add-hook 'erc-send-modify-hook     #'irc-e-maybe-send-fixup nil t)
        (add-hook 'erc-send-completed-hook  #'irc-e-post-send nil t)
        (add-hook 'erc-insert-pre-hook      #'irc-e-pre-insert nil t)
        (add-hook 'erc-insert-modify-hook   #'irc-e-maybe-insert nil t)
        (add-hook 'erc-insert-post-hook     #'irc-e-dh-post-insert nil t)

        ;; Reset buffer locals
        (setq irc-e--left-over    nil
              irc-e--insert-queue nil
              irc-e-fill-function erc-fill-function
              erc-fill-function   nil
              erc-query-display   'bury
              erc-auto-query      'bury))

    ;; Disabled
    (progn
      (remove-hook 'erc-send-pre-hook       #'irc-e-maybe-send t)
      (remove-hook 'erc-send-modify-hook    #'irc-e-maybe-send-fixup t)
      (remove-hook 'erc-send-completed-hook #'irc-e-post-send t)
      (remove-hook 'erc-insert-pre-hook     #'irc-e-pre-insert t)
      (remove-hook 'erc-insert-modify-hook  #'irc-e-maybe-insert t)
      (remove-hook 'erc-insert-post-hook    #'irc-e-dh-post-insert t)
      (setq erc-fill-function irc-e-fill-function
            irc-e-fill-function nil))))

;;; MODE


(defun irc-e-find-key ()
  "Find key file and enable irc-e when found."
  (when (and (derived-mode-p  'erc-mode)
             (erc-default-target))
    (let* ((channel   (erc-default-target))
           (target    (when   (string= (substring channel 0 1) "#") t))
           (key-path  (format "%sservers/%s/%s/%s/%s"
                              irc-e--dir ;dir tree of irc-e - 1st %s
                              (erc-network-name) ;connected server - 2nd %s
                              (if (eq target t);<--if channel search in
                                  "channels";<- 'channels' dir, else search in
                                "ircers"); personal 'ircers' directory - 3rd %s
                              channel;<--- channel or friend directory - 4rd %s
                              channel));<- channel or friend content
           (key-exists (file-exists-p key-path)));<- if key found
      (when key-exists (progn (irc-e-enable);<-- then enable irc-e
                              (setq irc-e-key-file key-path))))));<- set
;;                                                               path of key


(defun irc-e-on-off-check (string)
  "Enable irc-e if '----CRYPT ON----' string found in STRING.
Needed for receiving public keys and signature."
  (unless irc-e-mode
    (when (eq major-mode 'erc-mode)
      (when (string-match "----CRYPT ON----" string)
        (irc-e-enable)))))

;;; MISC

(defun irc-e--message (format-string &rest args)
  "Call `message' with FORMAT-STRING and ARGS."
  (let ((message-truncate-lines t))
    (message "irc-e: %s" (apply #'format format-string args))))



;;; TEXT RESTORE

(defun irc-e-prefix-check ()
  "Set prefix of received message based on `irc-e-msg-type' variable.
The variable is set in `irc-e-string-check' function always when
 received message."
  (let ((prefix (cond ((string= irc-e-msg-type "normal-encrypted")
                irc-e-prefix)
               ((string= irc-e-msg-type "x25519_pub")
                irc-e-dh-prefix)
               ((string= irc-e-msg-type "ed25519_pub")
                irc-e-ed-pubkey-prefix)
               ((string= irc-e-msg-type "ed25519_sig")
                irc-e-ed-sig-prefix))))
    prefix))

(defun irc-e-postfix-check ()
    "Set postfix of received message based on `irc-e-msg-type' variable.
The variable is set in `irc-e-string-check' function always when
 received message."
  (let ((postfix (cond ((string= irc-e-msg-type "normal-encrypted")
                        irc-e-postfix)
                       ((string= irc-e-msg-type "x25519_pub")
                        irc-e-dh-postfix)
                       ((string= irc-e-msg-type "ed25519_pub")
                        irc-e-ed-pubkey-postfix)
                       ((string= irc-e-msg-type "ed25519_sig")
                        irc-e-ed-sig-postfix))))
    postfix))

(cl-defmacro irc-e--with-message ((message) &rest body)
  "Conveniently work with narrowed region as implemented by ERC hooks.

Search for and extract an encrypted message (if present),
then bind MESSAGE to it, delete the encrypted string from buffer
and execute BODY.  Finally, restore ERC text properties.

See `erc-send-modify-hook' and `erc-insert-modify-hook'."
  (declare (indent defun))
  (let ((start (cl-gensym)))
    `(when irc-e-mode
       (goto-char (point-min))
       (let* ((prefix (irc-e-prefix-check))
              (postfix (irc-e-postfix-check))
              (,start nil))
         (when (re-search-forward (concat prefix ".+" postfix) nil t)
           (let ((,message (buffer-substring (+ (match-beginning 0)
                                                (length prefix))
                                             (- (match-end 0)
                                                (length postfix))))
                 (,start (match-beginning 0)))
             (unless (string= irc-e-msg-type "plain-text")
               (delete-region (match-beginning 0) (match-end 0)))
               (goto-char ,start)
             ,@body)
           (erc-restore-text-properties))))))


;;; ENCRYPTION, SPLITTING AND PADDING

(defun irc-e-split-message (string)
  "Split STRING and pad to maximum size if needed."
  (let* ((len (length string)))
    (cond ((<= len irc-e-max-length)
           ;; Pad to maximum size if needed
           (irc-e--pad (list string)))
          (t
           (irc-e--pad (irc-e--split string))))))

(defun irc-e--generate-iv ()
  "Generate a suitable IV to be used for message encryption.
Return IV as a 128bit hex string."
  (let ((iv (substring
             (secure-hash
              'sha256 ;; (with-temp-buffer
                        ;; (insert-file-contents-literally
                        ;;  "/dev/urandom" nil nil 1000)
              (number-to-string (random t)))
                        ;;(buffer-string)))
             0 32)))
    iv))


(defun irc-e--pad (list)
  "Pad message or fragments in LIST to `irc-e-max-length' bytes.
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
                       (diff (- irc-e-max-length len))
                       (pad  (cl-loop repeat diff concat
                                      (string (random 256)))))
                  (concat string pad (string split-tag) "@" (string len)))))
    (cl-loop for (msg . rest) on list
             if rest collect (do-pad msg 1)
             else collect    (do-pad msg 0))))


(defun irc-e--split (string)
  "Split STRING into substrings that are at most `irc-e-max-length' long.
Splitting does not take into account word boundaries or whitespace.

Return list of substrings."
  (cl-loop with len   = (length string)
           with start = 0
           with max   = irc-e-max-length
           while (< start len)
           collect (substring string start (min len (cl-incf start max)))))


(cl-defun irc-e-encrypt (string)
  "Encrypt STRING with `irc-e-key'.
An IV generated dynamically by `irc-e--generate-iv' is used for encryption.

If `irc-e-key' is nil, ask for a key interactively.

Return BASE64 encoded concatenation of IV and CIPHERTEXT which should be
BASE64 encoded as well.  Return nil on all errors."
  (unless irc-e-key-file
    (irc-e--message "Encryption key not found"))
  (condition-case ex
      (let ((iv  (irc-e--generate-iv))
            (key irc-e-key-file))
        (cl-multiple-value-bind (status result)
            (with-temp-buffer
              (insert (encode-coding-string string 'utf-8))
              (list (call-process-region
                     (point-min) (point-max)
                     irc-e-openssl-path t '(t nil) nil
                     "enc" "-a" (concat "-" irc-e-cipher)
                     "-iv" iv "-kfile"
                     key "-salt")
                    (buffer-string)))
          (unless (= status 0)
            (irc-e--message "Output: %s" result)
            (irc-e--message
             "Non-zero return code %s from openssl (encrypt)" status)
            (cl-return-from irc-e-encrypt nil))
          (base64-encode-string (concat iv result) t)))
    ('error
     (irc-e--message "%s (process error/irc-e-encrypt)"
                         (error-message-string ex))
     nil)))



(cl-defun irc-e-decrypt (string)
  "Decrypt STRING with keyfile found at `irc-e-key-file'.
the IV as a 16 byte hex string
and the CIPHERTEXT, which must be BASE64 encoded as well."
  (unless irc-e-key-file
    (irc-e--message "No key set, could not decrypt")
    (cl-return-from irc-e-decrypt nil))
  (condition-case ex
      (let* ((str (base64-decode-string string))
             (iv  (substring str 0 32))
             (ciphertext (substring str 32))
             (key irc-e-key-file)) ; because 'irc-e-key-file' is buffer
        (cl-multiple-value-bind (status result) ; local then after buffer is
            (with-temp-buffer                   ; switch to temp buffer
              (insert ciphertext)               ; 'irc-e-key-file' variable
              (list (call-process-region        ; is not available. Then it
                     (point-min) (point-max)    ; must be passed as local
                     irc-e-openssl-path t '(t nil) nil     ; variable
                     "enc" "-d" "-a" (concat "-" irc-e-cipher)
                     "-iv" iv "-kfile"
                     key "-salt")
                    (buffer-string)))
          (unless (= status 0)
            (irc-e--message
             "Non-zero return code %s from openssl (irc-e-decrypt)" status)
            (cl-return-from irc-e-decrypt nil))
          result))
    ('error
     (irc-e--message "%s (process error/irc-e-decrypt)"
                         (error-message-string ex))
     nil)))


;;; SENDING

(defun irc-e-maybe-send (string)
  "Encrypt STRING and send to receiver.  Run as a hook in `erc-send-pre-hook'.
STRING should contain user input.  In order to get around IRC protocol
message size limits, STRING is split into fragments and padded to a
constant size, `irc-e-max-length', by calling `irc-e-split-message'.
The resulting padded fragments are encrypted and sent separately,
the original message reconstructed at the receiver end, with the original
formatting preserved intact.

On errors, do not send STRING to the server."
  (when (and irc-e-mode irc-e-key-file
             ;; Skip ERC commands
             (not (string= "/" (substring string 0 1))))
    (let* ((split     (irc-e-split-message string))
           (encrypted (mapcar #'irc-e-encrypt split)))
      (cond ((cl-some #'null encrypted)
             (irc-e--message "Message will not be sent")
             (setq erc-send-this nil))
            (t
             ;; str is dynamically bound
             (defvar str nil)
             (setq irc-e-message str
                   str (concat irc-e-prefix
                               (cl-first encrypted)
                               irc-e-postfix)
                   irc-e--left-over
                   (cl-rest encrypted)))))))


(defun irc-e-maybe-send-fixup ()
  "Restore encrypted message back to its plaintext form.
This happens inside `erc-send-modify-hook'."
  (irc-e--with-message (msg)
    (insert irc-e-message)
    (goto-char (point-min))
    (insert (concat (propertize irc-e-indicator 'face
                                (list :foreground irc-e-success-color))
                    " "))))

;;; RECEIVING

(defun irc-e-string-check (string)
  "Check type of irc message in STRING for irc-e."
  (cond ((string-match (concat irc-e-dh-prefix "\\(.+\\)"
                               irc-e-dh-postfix)
                       string) ;; if string is a public key
         (setq irc-e-msg-type "x25519_pub")
         (irc-e-dh-receiver string))

        ((string-match (concat irc-e-ed-pubkey-prefix "\\(.+\\)"
                               irc-e-ed-pubkey-postfix)
                       string)  ;; if string is public key for signature
         (setq irc-e-msg-type "ed25519_pub")
         (irc-e-dh-receiver string))
        
        ((string-match (concat irc-e-ed-sig-prefix "\\(.+\\)"
                               irc-e-ed-sig-postfix)
                       string) ;; if string is a sign of public key
         (setq irc-e-msg-type "ed25519_sig")
         (irc-e-dh-receiver string))

        ((string-match (concat irc-e-prefix "\\(.+\\)"
                               irc-e-postfix)
                       string) ;; if string is normal encrypted string
         (setq irc-e-msg-type "normal-encrypted"))

        (t
         (setq irc-e-msg-type "plain-text"))));<-- if none above

(defun irc-e-dh-receiver (string)
  "Decode dh key from STRING and later reconstruct fragments if its in parts."
  (let* ((strb64 (match-string 1 string)) ;; get matched string
         (dec64  (base64-decode-string strb64)) ;; decode string from base64
         (len    (length dec64)) ;; needed for remove split tag
         (split  (aref dec64 (- len 1)))
         (final  (substring dec64 0 (- len 1))))
    (push (cons  final split) irc-e--insert-queue)
    (when        (= split 1) (setq erc-insert-this nil))))

(defun irc-e-pre-insert (string)
  "Decrypt STRING and insert it into `irc-e--insert-queue'.
If decrypted message is a fragment, `erc-insert-this' is set to nil.
Does not display message and does not trigger `erc-insert-modify-hook'."
  (irc-e-string-check string)
  (when (string=  irc-e-msg-type "normal-encrypted")
    (let*   ((msg       (match-string 1 string))
             (decrypted (irc-e-decrypt msg)))
      (if     decrypted
          (let* ((len       (length decrypted))
                 (split     (aref decrypted (- len 3)));<- python
                 (original  (aref decrypted (- len 1)));<- compatible
                 (decrypted (substring decrypted 0 original)))
            (push (cons decrypted split) irc-e--insert-queue)
            (if (= split 1) (setq erc-insert-this nil)))
        ;; Error, erc-insert-this will be set to t so it's not possible
        ;; for multiple error-indicating conses to be inserted in the
        ;; queue.
        (push (cons :error nil) irc-e--insert-queue)))))


(defun irc-e--insert (msg &optional error)
  (insert (concat (if error "(decrypt error) " "")
                  (decode-coding-string msg 'utf-8 :nocopy)))
  (goto-char (point-min))
  (unless (string= irc-e-msg-type "plain-text")
    (insert (concat
             (propertize
              irc-e-indicator 'face
              (list :foreground
                    (if error
                        irc-e-failure-color irc-e-success-color)))
             " ")))
  (setq irc-e--insert-queue nil))


(defun irc-e-maybe-insert ()
  "Display decrypted messages and do fragment reconstruction.
This happens inside `erc-insert-modify-hook'."
  (irc-e--with-message (msg)
    (cl-loop with first = (cl-first irc-e--insert-queue)
             with rest  = (cl-rest irc-e--insert-queue)
             with msg   = (car first)
             with tag   = (cdr first)
             ;; Incomplete message fragment
             when (equal tag 1)
             do (cl-return)
             ;; Complete message in one fragment
             when (and (equal tag 0)
                       (null rest))
             do (irc-e--insert msg)
             (setq irc-e--post-insert msg)
             (cl-return)
             ;; Either an error or final fragment
             for fragment in rest collect (car fragment) into out
             finally
             (let ((out (mapconcat #'identity (nreverse out) "")))
               (if (eql msg :error)
                   (irc-e--insert out t)
                 (setq irc-e--post-insert (concat out msg))
                 (irc-e--insert (concat out msg)))))))

;;; KEY EXCHANGE

(defun irc-e-dir-check (dir)
  "Check if DIR exists and if not make it."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun irc-e-sig-b64-convert (tempdir nick)
  "Convert ed25519_sig.bin binary in TEMPDIR with NICK in filename."
  (let ((sig (concat tempdir nick)))
    (call-process
     "base64" nil ;; no infile
     `(:file ,(concat sig "-ed25519_sig.bin"))
     nil "-d" (concat sig "-ed25519_sig.b64"))
    (delete-file (concat sig "-ed25519_sig.b64"))))

(defun irc-e-dh-key-exist-p (tempdir nick)
  "Check if all needed keys in TEMPDIR with NICK in filename exists."
  (when (and (file-exists-p (concat tempdir nick "-ed25519_sig.bin"))
             (file-exists-p (concat tempdir nick "-ed25519_pub.pem"))
             (file-exists-p (concat tempdir nick "-x25519_pub.pem")))
    t))

(defun irc-e-dh-key-save (tempdir nick key-or-sig keytype)
  "Save received KEY-OR-SIG in TEMPDIR with NICK and type in filename."
  
  ;;   because ed25519_sig is binary file it must be saved to text file and
  (if (string= keytype "ed25519_sig");then converted from base64
      (progn (with-temp-file                ;by separate process
                 (concat tempdir nick "-ed25519_sig.b64")
               (insert
                key-or-sig))
             (irc-e-sig-b64-convert tempdir nick))
    (with-temp-file (concat tempdir nick "-" keytype ".pem")
      (insert key-or-sig))))


(defun irc-e-dh-post-insert ()
  "Save received public x25519 key."
  (when (and (or  (string= irc-e-msg-type "x25519_pub") ; don't save keys
                  (string= irc-e-msg-type "ed25519_sig") ; received on public
                  (string= irc-e-msg-type "ed25519_pub")) ; channel
             (not (string= "#" (substring (erc-default-target) 0 1)))
             (not (string= irc-e-msg-type "plain-text")))
    (let* ((keytype     irc-e-msg-type)
           (key-or-sig  irc-e--post-insert);<---- received key or signature
           (nick        (irc-e-get-last-message-nick))
           (network     (erc-network-name))
           (dir         (format
                         "%sservers/%s/ircers/%s/" ;; <---- directory tree with
                         irc-e--dir ;                                   pubkeys
                         network ; directory of connected server
                         nick)) ;; dir of friend if when /query
           (tempdir      (concat irc-e--dir "temp/" nick))
           (tempdir      (file-name-as-directory tempdir)))
      ;;                   temporary dir for not yet verified keys

      (irc-e-dir-check dir);<--- make personal ircer directory and
      (irc-e-dir-check tempdir);<- temporary dir for verifying
      
      (irc-e-dh-key-save tempdir nick key-or-sig keytype) ;; save received keys

      (when (irc-e-dh-key-exist-p tempdir nick) ;; make sure needed keys and
        ;;                                               signature are saved
        (let ((status (irc-e-verify-sig tempdir nick)))
          (if (= status 0) ;; save new keys only if signature verified positive
              (progn
                (irc-e--message "Received x25519 and ed25519 keys.\
 Signature verified successfully.")
                (irc-e-archive-keys dir)
                ;;                        make shared secret file
                (irc-e-dh-secret tempdir nick);<---for encryption
                ;;
                (irc-e-move-keys tempdir dir)) ;now when old keys moved to
                ;;                              archive move verified keys and
                ;;                              signature to personal ircer
                ;;                              directory
            (irc-e--message "Received x25519 and ed25519 keys with signature,\
 but verify failed."))))))
  
  (setq irc-e-msg-type nil
        irc-e--post-insert nil))


(defun irc-e-archive-keys (dir)
  "Archive previous keys found.  DIR is personal ircer directory."
  (irc-e-dir-check (concat dir "/archive"))
  (when (directory-files dir) ; if old keys found move them
    (irc-e-move-keys          ; to archive - currently only
     dir                      ; one keypairs are archived
     (concat dir "/archive/"))))

      

(defun irc-e-move-keys (dir1 dir2)
  "Move public keys located in DIR1 to DIR2 directory."
  (cl-loop for keyfile in (directory-files dir1) do
           (unless (or (string= keyfile ".")  ;; omit currenct dir
                       (string= keyfile "..") ;; omit parent dir
                       (string= keyfile "archive"));  omit archive dir
             (copy-file (concat dir1 "/" keyfile) dir2 t) ;copy content
             (delete-file (concat dir1 "/" keyfile))))) ; of dir passed as
;;           remove content of tempdir                           as option

(defun irc-e-verify-sig (tempdir nick)
  "Verify received ed25519 signature, then check verified status in TEMPDIR.
If positive copy to DIR / NICK directory and
delete after original ~/.emasc.d/irc/irc-e/temp/verify-status after all."
  (let* ((keyname (concat tempdir nick))
         (status  (call-process
                   irc-e-openssl-path
                   nil ;; no infile
                   nil ;; no output
                   nil
                   "pkeyutl" "-verify" "-pubin" "-inkey"
                   (concat keyname "-ed25519_pub.pem")
                   "-rawin" "-in" (concat keyname "-x25519_pub.pem")
                   "-sigfile" (concat keyname "-ed25519_sig.bin"))))
    status))

(defun irc-e-dh-secret (tempdir nick)
  "Make shared secret for encrytpion in TEMPDIR.
Then move to KEYFILE with filename NICK."
  (if (nthcdr 3 (directory-files (concat irc-e--dir "secret/")))
      (call-process irc-e-openssl-path
                    nil ;; no infile
                    `(:file ,(concat tempdir nick)) ;; output
                    nil "pkeyutl" "-derive" "-inkey"
                    (concat irc-e--dir "secret/x25519_priv.pem")
                    "--peerkey" (concat tempdir nick "-x25519_pub.pem")
                    "-out" (concat tempdir nick))
    (irc-e--message (format "Received and verified x25519 and ed25519 keys with \
signature from %s, but own keypairs and not found. \
Generate them with /crypt genkeys first" nick))))

(defun irc-e-get-last-message-nick ()
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
             (nick       (match-string 0 nick-line)) ;; bind nick to variable
             (nick       (substring nick 1 (- (length nick) 1))))
             (goto-char  (point-max))
        nick))))
;; return nick

(defun irc-e-post-send (string)
  "Send message fragments placed in `irc-e--left-over' to remote end.
STRING is unused, but required."
  (unwind-protect
      (cl-loop for m in irc-e--left-over do
            (erc-message "PRIVMSG"
                         (concat (erc-default-target) " "
                                 (concat irc-e-prefix m irc-e-postfix))
                         ))
    (setq irc-e--left-over nil)))

(defun irc-e--dh-split (string)
  "Split key or signature in STRING."
  (cl-loop with len   = (length string)
           with start = 0
           with max   = 150
           while (< start len)
           collect (substring string start (min len (cl-incf start max)))))

(defun irc-e--dh-tag (list)
  "Tag fragments of key in LIST.
Without padding because it is unneeded when sending keys."
  (cl-labels ((do-tag (string dh-tag)
                (concat string (string dh-tag))))
    (cl-loop for (msg . rest) on list
             if rest collect (do-tag msg 1)
             else collect    (do-tag msg 0))))

(defun irc-e-split-dh (string)
  "Firstly split key in STRING, then tag parts."
  (irc-e--dh-tag (irc-e--dh-split string)))

(defun irc-e-dh-pubkey-read ()
  "Read x25519_pub.pem key found in `irc-e--dir'/secret/ directory.
It is used for make shared secret file for encrypting messages."
    (with-temp-buffer
      (insert-file-contents (concat irc-e--dir "secret/x25519_pub.pem"))
      (let* ((dh-split (irc-e-split-dh (buffer-string)))
             (dh-b64   (mapcar #'(lambda (dh-split)
                                 (base64-encode-string dh-split t))
                             dh-split)))
    dh-b64)))

(defun irc-e-dh-ed-pubkey-read ()
  "Read ed25519_pub.pem key found in `irc-e--dir'/secret/ directory.
It is used for verify x25519_pub.pem key."
    (with-temp-buffer
    (insert-file-contents (concat
                           irc-e--dir "secret/ed25519_pub.pem"))
    (let* ((ed-split (irc-e-split-dh (buffer-string)))
           (ed-b64   (mapcar #'(lambda (ed-split)
                               (base64-encode-string ed-split t))
                           ed-split)))
    ed-b64)))

(defun irc-e-dh-ed-sig-read ()
  "Read ed25519_sig.bin signature file found in `irc-e--dir'/secret/ dir.
It is used for verify x25519_pub.pem key."
    (with-temp-buffer
    (insert-file-contents (concat
                           irc-e--dir "secret/ed25519_sig.bin"))
    (let* ((sig-b64   (base64-encode-string (buffer-string)))
           (sig-split (irc-e-split-dh sig-b64))
           (sig-b64   (mapcar #'(lambda (sig-split)
                                (base64-encode-string sig-split t))
                            sig-split)))
    sig-b64)))

(defun irc-e-dh-ex (nick)
  "Read and public keys and signature and send to NICK."
  (let ((dh-b64  (irc-e-dh-pubkey-read))
        (ed-b64  (irc-e-dh-ed-pubkey-read))
        (sig-b64 (irc-e-dh-ed-sig-read)))


  (erc-message "PRIVMSG" (concat (car nick) " " "----" "CRYPT ON" "----"))
  
  (irc-e-send-signed-pub (car nick)
                             irc-e-dh-prefix
                             dh-b64
                             irc-e-dh-postfix)
  
  (irc-e-send-signed-pub (car nick)
                             irc-e-ed-pubkey-prefix
                             ed-b64
                             irc-e-ed-pubkey-postfix)
  
  (irc-e-send-signed-pub (car nick)
                             irc-e-ed-sig-prefix
                             sig-b64
                             irc-e-ed-sig-postfix)))

                               
(defun irc-e-send-signed-pub (nick prefix-type signed_pub postfix-type)
  "Send keys to NICK with postfix and prefix suggestive type of message.
PREFIX-TYPE like POSTFIX-TYPE suggest type of message, and SIGNED_PUB is signed
x25519 public key."
  (unwind-protect
      (cl-loop for dh-part in signed_pub do
               (erc-message "PRIVMSG" (concat nick " "
                                              prefix-type dh-part
                                              postfix-type )))))
  
(defun irc-e-dh-multi-ex (nicks)
  "Send public keys to NICKS."
  (cl-loop for nick in nicks do
           (irc-e-dh-ex (list nick)))) ;; irc-e-dh-ex expect list because
;;                                        &rest keyword in erc-cmd-CRYPT

(defun irc-e-dir-clean (dir)
  "Remove all keys in DIR."
  (cl-loop for file in (directory-files dir) do
           (unless (or (string= file ".")
                       (string= file ".."))
           (delete-file (concat dir "/" file)))))

(defun irc-e-dh-generate-keys ()
  "Generate own public and private x25519 keys."
  (irc-e-dir-check "~/.emacs.d/irc/irc-e/secret")
  (when (directory-files "~/.emacs.d/irc/irc-e/secret/")
    (irc-e-dir-clean "~/.emacs.d/irc/irc-e/secret/"))

  (let* ((secret-dir (concat (concat irc-e--dir "secret")))  ;  
         (status-priv (call-process irc-e-openssl-path nil ; no infile
                                    `(:file ,(concat secret-dir ;destination
                                                     "/x25519_priv.pem"));-^
                                    nil ;; don't display result
                                    "genpkey" "-algorithm" "x25519" "-out"
                                    (concat secret-dir "/x25519_priv.pem")))
         
         (status-pub (call-process
                      irc-e-openssl-path nil ; no infile   |    destination
                      `(:file ,(concat secret-dir "/x25519_pub.pem"));<---^
                      nil ;; don't display result
                      "pkey" "-in" (concat secret-dir "/x25519_priv.pem")
                      "-pubout" "-out" (concat secret-dir "/x25519_pub.pem"))))
    (if (and (= status-priv 0)
             (= status-pub  0)) ;; if everything OK
        (irc-e-generate-ed-sig-keys secret-dir)
      (cond ((not (= status-priv 0))
             (irc-e--message (concat "Generate " secret-dir
                                         "x25519_priv.pem key failed.")))
            ((not (= status-pub 0))
             (irc-e--message (concat "Generate " secret-dir
                                         "x25519_pub.pem key failed.")))))))


(defun irc-e-generate-ed-sig-keys (secret-dir)
  "Generate public and private signing ed25519 keys in SECRET-DIR."
  (let ((ed-status-priv (call-process
                      irc-e-openssl-path
                      nil `(:file ,(concat secret-dir "/ed25519_priv.pem"))
                      nil "genpkey" "-algorithm" "Ed25519" "-out"
                      (concat secret-dir "/ed25519_priv.pem")))

        (ed-status-pub (call-process
                     irc-e-openssl-path
                     nil `(:file ,(concat secret-dir "/ed25519_pub.pem"))
                     nil "pkey" "-in" (concat secret-dir "/ed25519_priv.pem")
                     "-pubout" "-out" (concat secret-dir "/ed25519_pub.pem"))))
    (if (and (= ed-status-priv 0)
             (= ed-status-pub  0)) ;; if everything OK
        (irc-e-ed-keysign secret-dir)
      (cond ((not (= ed-status-priv 0))
             (irc-e--message (concat "Generate " secret-dir
                                         "ed25519_priv.pem key failed.")))
            ((not (= ed-status-pub 0))
             (irc-e--message (concat "Generate " secret-dir
                                         "ed25519_pub.pem key failed."))))))
  (irc-e-ed-keysign secret-dir))


(defun irc-e-ed-keysign (secret-dir)
  "Sign keys found in SECRET-DIR."
  (let ((ed-status-sig (call-process
                     irc-e-openssl-path
                     nil `(:file ,(concat secret-dir "/ed25519_sig.bin"))
                     nil "pkeyutl" "-sign" "-inkey"
                     (concat secret-dir "/ed25519_priv.pem")
                     "-out" (concat secret-dir "/ed25519_sig.bin")
                     "-rawin" "-in" (concat secret-dir "/x25519_pub.pem"))))
    (if (= ed-status-sig 0)
        (irc-e--message "Keypairs generated and signed succesfully.")
      (irc-e--message "Generate keypairs or signing failed."))))


(defun erc-cmd-CRYPT (&rest args)
  "Erc irc-e command to generate keys, sign them, send to friend with ARGS."
  (let ((enabled (when irc-e-mode t))
        (option  (car args)))
    (cond ((string= option "genkeys")
           (irc-e-dh-generate-keys))
          ;;
          ;; send public keys to all
          ;; on channel in privmsg
          ((string= option "dh-all")
           (irc-e-dh-multi-ex (erc-get-channel-nickname-list)))
          ;;
          ;; send public key to nick
          ((string= option "dh")
           (cond ((> (length (cdr args)) 1)
                  (irc-e-dh-multi-ex args))
                 ((= 1 (length (cdr args)))
                  (irc-e-dh-ex (cdr args)))
                 ((eq  (cdr args) nil)
                  (irc-e--message "nick or nicks required required"))))
          ;;
          ;; enable irc-e
          ((string= option "on")
           (irc-e-enable))
          ;;
          ;; disable irc-e
          ((string= option "off")
           (irc-e-disable))
          
          ;; send clear text message
          ((string= option "ct")
           (irc-e-disable)
           (let ((len (length (format "%s" args))))
             (erc-send-input (substring (format "%s" args) 1 (- len 1))))
           (irc-e-enable)))))

;;;
;;; Interactive
;;;


;;;###autoload
(defun irc-e-enable ()
  "Enable PSK encryption for the current buffer."
  (interactive)
  (when (eq major-mode 'erc-mode) t)
  (irc-e-mode t))

;;;###autoload
(defun irc-e-disable ()
  "Disable PSK encryption for the current buffer."
  (interactive)
  (when (eq major-mode 'erc-mode) t)
  (irc-e-mode -1))

(provide 'irc-e)
;;; irc-e.el ends here
