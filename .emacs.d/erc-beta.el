;; TINYURL
;; To create a URL shortener in GNU Emacs, you can use the url-retrieve function to send HTTP requests and retrieve data from a URL shortening service. Here's an example of how you can create a simple URL shortener using the https://tinyurl.com API:
(defun shorten-url (long-url)
	"Shorten a URL using the https://tinyurl.com API."
	(let ((url-request-method "POST")
				(url-request-extra-headers
					`(("Content-Type" . "application/x-www-form-urlencoded")
						("Content-Length" . ,(format "%d" (length long-url))))))
		(with-current-buffer (url-retrieve-synchronously "https://tinyurl.com/api-create.php" t t)
												 (goto-char (point-min))
												 (insert long-url)
												 (delete-region (point-min) (line-end-position))
												 (buffer-string))))
;;In this example, the shorten-url function takes a long-url argument and returns a shortened URL using the TinyURL API. The function sends a POST request to the https://tinyurl.com/api-create.php URL with the long-url as the body of the request. The response from the API is retrieved using url-retrieve-synchronously, which returns a buffer containing the response. The function then extracts the shortened URL from the buffer and returns it as a string.

;;To use the shorten-url function, you can call it with a long URL as the argument:
(shorten-url "https://www.gnu.org/software/emacs/")
;; This will return a shortened URL, such as https://tinyurl.com/abc123. Note that you will need an active internet connection for the function to work, as it sends a request to the TinyURL API.


;;PASTEBIN

;;To use a pastebin service in GNU Emacs, you can use the pastebinit command-line tool, which allows you to upload a file or buffer to a pastebin service and get a URL that you can share with others.

;;Here's an example of how to use pastebinit to upload the contents of a buffer:

(defun pastebin-buffer ()
	"Upload the current buffer to a pastebin service using pastebinit."
	(interactive)
	(let ((output-buffer (generate-new-buffer "*pastebinit*")))
		(shell-command-on-region (point-min) (point-max) "pastebinit" output-buffer)
		(with-current-buffer output-buffer
												 (goto-char (point-min))
												 (let ((url (buffer-substring (point) (line-end-position))))
													 (message "Paste uploaded to: %s" url))))))
;;In this example, the pastebin-buffer function uploads the contents of the current buffer to a pastebin service using pastebinit. The function creates a new buffer called "pastebinit" to capture the output of the command. It then uses shell-command-on-region to run the pastebinit command on the region containing the contents of the current buffer, and sends the output to the "pastebinit" buffer. The function then extracts the URL of the uploaded paste from the output buffer and displays it in the minibuffer.

;;To use the pastebin-buffer function, simply call it using M-x pastebin-buffer. The function will upload the contents of the current buffer to a pastebin service and display the URL in the minibuffer.

;;You can customize the pastebin service used by pastebinit by setting the PASTEBINIT_PASTE_SERVICE environment variable. For example, to use the "Ubuntu Pastebin" service, you can add the following line to your Emacs configuration file:
(setenv "PASTEBINIT_PASTE_SERVICE" "https://paste.ubuntu.com/")

;;Note that you will need to have pastebinit installed on your system for this function to work. You can install it on Debian/Ubuntu using apt-get install pastebinit, or on other systems using your package manager of choice.

;;HTTP UPLOAD
;;To perform HTTP uploads from GNU Emacs, you can use the url-retrieve-synchronously or url-retrieve functions to send an HTTP POST request with the contents of a file or buffer.

;;Here's an example of how to upload a file to a web server using url-retrieve-synchronously:

(defun http-upload-file (file url)
	"Upload a file to a web server using HTTP POST."
	(let ((url-request-method "POST")
				(url-request-extra-headers
					'(("Content-Type" . "application/octet-stream")))
				(url-request-data
					(with-temp-buffer
						(insert-file-contents-literally file)
						(buffer-string))))
		(with-current-buffer (url-retrieve-synchronously url t t)
												 (buffer-string))))

;;In this example, the http-upload-file function takes a file argument containing the path to the file to upload, and a url argument containing the URL of the web server to upload the file to. The function sets the url-request-method variable to "POST" to indicate that an HTTP POST request should be sent. It also sets the url-request-extra-headers variable to specify the content type of the upload data as "application/octet-stream". Finally, the function sets the url-request-data variable to the contents of the file to be uploaded.

;;The function then sends the HTTP POST request using url-retrieve-synchronously, which returns a buffer containing the response from the web server. The function returns the contents of the buffer as a string.

;;To use the http-upload-file function, simply call it with the path to the file to upload and the URL of the web server:
(http-upload-file "/path/to/file.txt" "https://example.com/upload")

;;This will upload the file to the specified URL and return the response from the server as a string.

;;Note that you can also use the url-retrieve function instead of url-retrieve-synchronously to perform the upload asynchronously, which can be useful for uploading large files or when you don't want to block Emacs while the upload is in progress. The url-retrieve function takes a callback function as an argument, which will be called with the response buffer when the upload is complete.

;;CHANNEL AND USER LISTS
;;In ERC, you can view the list of channels and users in a separate buffer using the C-c C-l keybinding. This will open the "Server buffer", which shows a list of channels and users on the IRC server.

;;To switch between the channel and user lists, use the TAB key. You can navigate the list using the C-n and C-p keys to move down and up, respectively.

;;To join a channel from the channel list, move the cursor to the channel name and press RET. To send a private message to a user, move the cursor to the user's name and press RET. This will open a new buffer for the channel or user's private messages.

;;You can also search the channel or user list by pressing C-s and entering the search term. This will highlight matching channel or user names in the list.

;;To close the Server buffer and return to the main ERC buffer, press C-c C-l again.

;;Note that the Server buffer is a read-only buffer, so you can't modify the channel or user list directly from the buffer. To join a new channel or send a private message to a new user, you'll need to do so from the main ERC buffer by typing the appropriate IRC commands.

(defun check-erc-limit ()
  "Check if the text in the current buffer exceeds the ERC limit of 510 characters."
  (interactive)
  (if (> (line-end-position) 510)
      (message "ERC limit exceeded!")
    (message "Within ERC limit.")))

