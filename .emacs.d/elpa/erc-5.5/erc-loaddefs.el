;;; erc-loaddefs.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "erc-autoaway" "erc-autoaway.el" "505c489cbfe5c01f958988716e7595c7")
;;; Generated autoloads from erc-autoaway.el
(autoload 'erc-autoaway-mode "erc-autoaway")

;;;### (autoloads "actual autoloads are elsewhere" "erc-autoaway"
;;;;;;  "erc-autoaway.el" (0 0 0 0))
;;; Generated autoloads from erc-autoaway.el

(register-definition-prefixes "erc-autoaway" '("erc-auto"))

;;;***

;;;***

;;;### (autoloads nil "erc-button" "erc-button.el" "ad1682a6b3ea4825dabf93b8faec2cb1")
;;; Generated autoloads from erc-button.el
(autoload 'erc-button-mode "erc-button" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-button" "erc-button.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-button.el

(register-definition-prefixes "erc-button" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-capab" "erc-capab.el" "dace4f0febf1900363adfdfaa200b00c")
;;; Generated autoloads from erc-capab.el
(autoload 'erc-capab-identify-mode "erc-capab" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-capab" "erc-capab.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-capab.el

(register-definition-prefixes "erc-capab" '("erc-capab-identify-"))

;;;***

;;;***

;;;### (autoloads nil "erc-compat" "erc-compat.el" "832e8e64db50e82052d9506d5f87bab6")
;;; Generated autoloads from erc-compat.el
(autoload 'erc-define-minor-mode "erc-compat")

;;;### (autoloads "actual autoloads are elsewhere" "erc-compat" "erc-compat.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-compat.el

(register-definition-prefixes "erc-compat" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-dcc" "erc-dcc.el" "11853e87692330bf56be76b962e8c570")
;;; Generated autoloads from erc-dcc.el
(autoload 'erc-dcc-mode "erc-dcc")

(autoload 'erc-cmd-DCC "erc-dcc" "\
Parser for /dcc command.
This figures out the dcc subcommand and calls the appropriate routine to
handle it.  The function dispatched should be named \"erc-dcc-do-FOO-command\",
where FOO is one of CLOSE, GET, SEND, LIST, CHAT, etc.

\(fn CMD &rest ARGS)" nil nil)

(autoload 'pcomplete/erc-mode/DCC "erc-dcc" "\
Provide completion for the /DCC command." nil nil)

(defvar erc-ctcp-query-DCC-hook '(erc-ctcp-query-DCC) "\
Hook variable for CTCP DCC queries.")

(autoload 'erc-ctcp-query-DCC "erc-dcc" "\
The function called when a CTCP DCC request is detected by the client.
It examines the DCC subcommand, and calls the appropriate routine for
that subcommand.

\(fn PROC NICK LOGIN HOST TO QUERY)" nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-dcc" "erc-dcc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-dcc.el

(register-definition-prefixes "erc-dcc" '("erc-" "pcomplete/erc-mode/"))

;;;***

;;;***

;;;### (autoloads nil "erc-desktop-notifications" "erc-desktop-notifications.el"
;;;;;;  "b860ac1b5154487f10131f4018a6db2a")
;;; Generated autoloads from erc-desktop-notifications.el
(autoload 'erc-notifications-mode "erc-desktop-notifications" "" t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-desktop-notifications"
;;;;;;  "erc-desktop-notifications.el" (0 0 0 0))
;;; Generated autoloads from erc-desktop-notifications.el

(register-definition-prefixes "erc-desktop-notifications" '("erc-notifications-"))

;;;***

;;;***

;;;### (autoloads nil "erc-ezbounce" "erc-ezbounce.el" "36abc762e49c4cada161aeb0767864f3")
;;; Generated autoloads from erc-ezbounce.el

(autoload 'erc-cmd-ezb "erc-ezbounce" "\
Send EZB commands to the EZBouncer verbatim.

\(fn LINE &optional FORCE)" nil nil)

(autoload 'erc-ezb-get-login "erc-ezbounce" "\
Return an appropriate EZBounce login for SERVER and PORT.
Look up entries in `erc-ezb-login-alist'.  If the username or password
in the alist is nil, prompt for the appropriate values.

\(fn SERVER PORT)" nil nil)

(autoload 'erc-ezb-lookup-action "erc-ezbounce" "\


\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-notice-autodetect "erc-ezbounce" "\
React on an EZBounce NOTICE request.

\(fn PROC PARSED)" nil nil)

(autoload 'erc-ezb-identify "erc-ezbounce" "\
Identify to the EZBouncer server.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-init-session-list "erc-ezbounce" "\
Reset the EZBounce session list to nil.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-end-of-session-list "erc-ezbounce" "\
Indicate the end of the EZBounce session listing.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-add-session "erc-ezbounce" "\
Add an EZBounce session to the session list.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-select "erc-ezbounce" "\
Select an IRC server to use by EZBounce, in ERC style.

\(fn MESSAGE)" nil nil)

(autoload 'erc-ezb-select-session "erc-ezbounce" "\
Select a detached EZBounce session." nil nil)

(autoload 'erc-ezb-initialize "erc-ezbounce" "\
Add EZBouncer convenience functions to ERC." nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-ezbounce"
;;;;;;  "erc-ezbounce.el" (0 0 0 0))
;;; Generated autoloads from erc-ezbounce.el

(register-definition-prefixes "erc-ezbounce" '("erc-ezb-"))

;;;***

;;;***

;;;### (autoloads nil "erc-fill" "erc-fill.el" "c226c318aaee3c64a1a8088a5b9eea83")
;;; Generated autoloads from erc-fill.el
(autoload 'erc-fill-mode "erc-fill" nil t)

(autoload 'erc-fill "erc-fill" "\
Fill a region using the function referenced in `erc-fill-function'.
You can put this on `erc-insert-modify-hook' and/or `erc-send-modify-hook'." nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-fill" "erc-fill.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-fill.el

(register-definition-prefixes "erc-fill" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-identd" "erc-identd.el" "407a32cd92b75b57661ce9e53d7e3e40")
;;; Generated autoloads from erc-identd.el
(autoload 'erc-identd-mode "erc-identd")

(autoload 'erc-identd-start "erc-identd" "\
Start an identd server listening to port 8113.
Port 113 (auth) will need to be redirected to port 8113 on your
machine -- using iptables, or a program like redir which can be
run from inetd.  The idea is to provide a simple identd server
when you need one, without having to install one globally on your
system.

\(fn &optional PORT)" t nil)

(autoload 'erc-identd-stop "erc-identd" "\


\(fn &rest IGNORE)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-identd" "erc-identd.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-identd.el

(register-definition-prefixes "erc-identd" '("erc-identd-"))

;;;***

;;;***

;;;### (autoloads nil "erc-imenu" "erc-imenu.el" "c5f2c4978d32e03f0feb6ebf7e6d7d73")
;;; Generated autoloads from erc-imenu.el

(autoload 'erc-create-imenu-index "erc-imenu" nil nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-imenu" "erc-imenu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-imenu.el

(register-definition-prefixes "erc-imenu" '("erc-unfill-notice"))

;;;***

;;;***

;;;### (autoloads nil "erc-join" "erc-join.el" "1b153962d3782b449be34db96c8b7b23")
;;; Generated autoloads from erc-join.el
(autoload 'erc-autojoin-mode "erc-join" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-join" "erc-join.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-join.el

(register-definition-prefixes "erc-join" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-list" "erc-list.el" "04cc21a00568a20e84c53eb6d8c16ab4")
;;; Generated autoloads from erc-list.el
(autoload 'erc-list-mode "erc-list")

;;;### (autoloads "actual autoloads are elsewhere" "erc-list" "erc-list.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-list.el

(register-definition-prefixes "erc-list" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-log" "erc-log.el" "8d4cfe3967b518c7ecbfb3be8a44053e")
;;; Generated autoloads from erc-log.el
(autoload 'erc-log-mode "erc-log" nil t)

(autoload 'erc-logging-enabled "erc-log" "\
Return non-nil if logging is enabled for BUFFER.
If BUFFER is nil, the value of `current-buffer' is used.
Logging is enabled if `erc-log-channels-directory' is non-nil, the directory
is writable (it will be created as necessary) and
`erc-enable-logging' returns a non-nil value.

\(fn &optional BUFFER)" nil nil)

(autoload 'erc-save-buffer-in-logs "erc-log" "\
Append BUFFER contents to the log file, if logging is enabled.
If BUFFER is not provided, current buffer is used.
Logging is enabled if `erc-logging-enabled' returns non-nil.

This is normally done on exit, to save the unsaved portion of the
buffer, since only the text that runs off the buffer limit is logged
automatically.

You can save every individual message by putting this function on
`erc-insert-post-hook'.

\(fn &optional BUFFER)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-log" "erc-log.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-log.el

(register-definition-prefixes "erc-log" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-match" "erc-match.el" "d3bc3887e1f84c9c5b8a202607682369")
;;; Generated autoloads from erc-match.el
(autoload 'erc-match-mode "erc-match")

(autoload 'erc-add-pal "erc-match" "\
Add pal interactively to `erc-pals'.

\(fn &optional ARG)" t nil)

(autoload 'erc-delete-pal "erc-match" "\
Delete pal interactively to `erc-pals'." t nil)

(autoload 'erc-add-fool "erc-match" "\
Add fool interactively to `erc-fools'.

\(fn &optional ARG)" t nil)

(autoload 'erc-delete-fool "erc-match" "\
Delete fool interactively to `erc-fools'." t nil)

(autoload 'erc-add-keyword "erc-match" "\
Add keyword interactively to `erc-keywords'.

\(fn &optional ARG)" t nil)

(autoload 'erc-delete-keyword "erc-match" "\
Delete keyword interactively to `erc-keywords'." t nil)

(autoload 'erc-add-dangerous-host "erc-match" "\
Add dangerous-host interactively to `erc-dangerous-hosts'.

\(fn &optional ARG)" t nil)

(autoload 'erc-delete-dangerous-host "erc-match" "\
Delete dangerous-host interactively to `erc-dangerous-hosts'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-match" "erc-match.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-match.el

(register-definition-prefixes "erc-match" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-menu" "erc-menu.el" "00efd7c9d18a164adb8291deb1abf1aa")
;;; Generated autoloads from erc-menu.el
(autoload 'erc-menu-mode "erc-menu" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-menu" "erc-menu.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-menu.el

(register-definition-prefixes "erc-menu" '("erc-menu-"))

;;;***

;;;***

;;;### (autoloads nil "erc-netsplit" "erc-netsplit.el" "dda4baa24ed153939a902a1095a3b776")
;;; Generated autoloads from erc-netsplit.el
(autoload 'erc-netsplit-mode "erc-netsplit")

(autoload 'erc-cmd-WHOLEFT "erc-netsplit" "\
Show who's gone." nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-netsplit"
;;;;;;  "erc-netsplit.el" (0 0 0 0))
;;; Generated autoloads from erc-netsplit.el

(register-definition-prefixes "erc-netsplit" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-notify" "erc-notify.el" "35dd53af082d9804b5db3a7844fa80ed")
;;; Generated autoloads from erc-notify.el
(autoload 'erc-notify-mode "erc-notify" nil t)

(autoload 'erc-cmd-NOTIFY "erc-notify" "\
Change `erc-notify-list' or list current notify-list members online.
Without args, list the current list of notified people online,
with args, toggle notify status of people.

\(fn &rest ARGS)" nil nil)

(autoload 'pcomplete/erc-mode/NOTIFY "erc-notify" nil nil nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-notify" "erc-notify.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-notify.el

(register-definition-prefixes "erc-notify" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-page" "erc-page.el" "6d74b47a32dbef6af4e7e1ba0011573f")
;;; Generated autoloads from erc-page.el
(autoload 'erc-page-mode "erc-page")

;;;### (autoloads "actual autoloads are elsewhere" "erc-page" "erc-page.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-page.el

(register-definition-prefixes "erc-page" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-pcomplete" "erc-pcomplete.el" "6fdf1e0d0dae940dd727811c93431ff0")
;;; Generated autoloads from erc-pcomplete.el
(autoload 'erc-completion-mode "erc-pcomplete" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-pcomplete"
;;;;;;  "erc-pcomplete.el" (0 0 0 0))
;;; Generated autoloads from erc-pcomplete.el

(register-definition-prefixes "erc-pcomplete" '("erc-pcomplet" "pcomplete"))

;;;***

;;;***

;;;### (autoloads nil "erc-replace" "erc-replace.el" "36cf9fa588dacdf6ff50d7f217e70a2e")
;;; Generated autoloads from erc-replace.el
(autoload 'erc-replace-mode "erc-replace")

;;;### (autoloads "actual autoloads are elsewhere" "erc-replace"
;;;;;;  "erc-replace.el" (0 0 0 0))
;;; Generated autoloads from erc-replace.el

(register-definition-prefixes "erc-replace" '("erc-replace-"))

;;;***

;;;***

;;;### (autoloads nil "erc-ring" "erc-ring.el" "1d0c249f214371b5a9c44b04fdc8534b")
;;; Generated autoloads from erc-ring.el
(autoload 'erc-ring-mode "erc-ring" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-ring" "erc-ring.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-ring.el

(register-definition-prefixes "erc-ring" '("erc-"))

;;;***

;;;***

;;;### (autoloads "actual autoloads are elsewhere" "erc-sasl" "erc-sasl.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-sasl.el

(register-definition-prefixes "erc-sasl" '("erc-sasl-"))

;;;***

;;;### (autoloads nil "erc-services" "erc-services.el" "19d021ef819d425b062e37c7b9add7f0")
;;; Generated autoloads from erc-services.el
(autoload 'erc-services-mode "erc-services" nil t)

(autoload 'erc-nickserv-identify-mode "erc-services" "\
Set up hooks according to which MODE the user has chosen.

\(fn MODE)" t nil)

(autoload 'erc-nickserv-identify "erc-services" "\
Identify to NickServ immediately.
Identification will either use NICK or the current nick if not
provided, and some password obtained through
`erc-nickserv-get-password' (which see).  If no password can be
found, an error is reported through `erc-error'.

Interactively, the user will be prompted for NICK, an empty
string meaning to default to the current nick.

Returns t if the identify message could be sent, nil otherwise.

\(fn &optional PASSWORD NICK)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-services"
;;;;;;  "erc-services.el" (0 0 0 0))
;;; Generated autoloads from erc-services.el

(register-definition-prefixes "erc-services" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-sound" "erc-sound.el" "d7167ece18bb318c148c85d4312997b0")
;;; Generated autoloads from erc-sound.el
(autoload 'erc-sound-mode "erc-sound")

;;;### (autoloads "actual autoloads are elsewhere" "erc-sound" "erc-sound.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-sound.el

(register-definition-prefixes "erc-sound" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-speedbar" "erc-speedbar.el" "dfc9c202dc9ae8562f8e7700b19fafad")
;;; Generated autoloads from erc-speedbar.el

(autoload 'erc-speedbar-browser "erc-speedbar" "\
Initialize speedbar to display an ERC browser.
This will add a speedbar major display mode." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-speedbar"
;;;;;;  "erc-speedbar.el" (0 0 0 0))
;;; Generated autoloads from erc-speedbar.el

(register-definition-prefixes "erc-speedbar" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-spelling" "erc-spelling.el" "05d434bfc777ccf4ac65ab1e2398bade")
;;; Generated autoloads from erc-spelling.el
(autoload 'erc-spelling-mode "erc-spelling" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-spelling"
;;;;;;  "erc-spelling.el" (0 0 0 0))
;;; Generated autoloads from erc-spelling.el

(register-definition-prefixes "erc-spelling" '("erc-spelling-"))

;;;***

;;;***

;;;### (autoloads nil "erc-stamp" "erc-stamp.el" "36672f82fa226f93caea77f2961b5ae8")
;;; Generated autoloads from erc-stamp.el
(autoload 'erc-timestamp-mode "erc-stamp" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-stamp" "erc-stamp.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-stamp.el

(register-definition-prefixes "erc-stamp" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-status-sidebar" "erc-status-sidebar.el"
;;;;;;  "954cd14170a91a4a6248374b7f486824")
;;; Generated autoloads from erc-status-sidebar.el

(autoload 'erc-status-sidebar-open "erc-status-sidebar" "\
Open or create a sidebar." t nil)

(autoload 'erc-status-sidebar-toggle "erc-status-sidebar" "\
Toggle the sidebar open/closed on the current frame." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-status-sidebar"
;;;;;;  "erc-status-sidebar.el" (0 0 0 0))
;;; Generated autoloads from erc-status-sidebar.el

(register-definition-prefixes "erc-status-sidebar" '("erc-status-sidebar-"))

;;;***

;;;***

;;;### (autoloads nil "erc-track" "erc-track.el" "8cb623646863ad6cfd255a5894ea5946")
;;; Generated autoloads from erc-track.el

(defvar erc-track-minor-mode nil "\
Non-nil if Erc-Track minor mode is enabled.
See the `erc-track-minor-mode' command
for a description of this minor mode.")

(custom-autoload 'erc-track-minor-mode "erc-track" nil)

(autoload 'erc-track-minor-mode "erc-track" "\
Toggle mode line display of ERC activity (ERC Track minor mode).

This is a minor mode.  If called interactively, toggle the
`Erc-Track minor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='erc-track-minor-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

ERC Track minor mode is a global minor mode.  It exists for the
sole purpose of providing the C-c C-SPC and C-c C-@ keybindings.
Make sure that you have enabled the track module, otherwise the
keybindings will not do anything useful.

\(fn &optional ARG)" t nil)
(autoload 'erc-track-mode "erc-track" nil t)

;;;### (autoloads "actual autoloads are elsewhere" "erc-track" "erc-track.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-track.el

(register-definition-prefixes "erc-track" '("erc-"))

;;;***

;;;***

;;;### (autoloads nil "erc-truncate" "erc-truncate.el" "e9eaf51dbf44a2496ab42fb6914b5ef4")
;;; Generated autoloads from erc-truncate.el
(autoload 'erc-truncate-mode "erc-truncate" nil t)

(autoload 'erc-truncate-buffer-to-size "erc-truncate" "\
Truncates the buffer to the size SIZE.
If BUFFER is not provided, the current buffer is assumed.  The deleted
region is logged if `erc-logging-enabled' returns non-nil.

\(fn SIZE &optional BUFFER)" nil nil)

(autoload 'erc-truncate-buffer "erc-truncate" "\
Truncates the current buffer to `erc-max-buffer-size'.
Meant to be used in hooks, like `erc-insert-post-hook'." t nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-truncate"
;;;;;;  "erc-truncate.el" (0 0 0 0))
;;; Generated autoloads from erc-truncate.el

(register-definition-prefixes "erc-truncate" '("erc-max-buffer-size"))

;;;***

;;;***

;;;### (autoloads nil "erc-xdcc" "erc-xdcc.el" "f422431579e757c9508f98a46311d9b0")
;;; Generated autoloads from erc-xdcc.el
(autoload 'erc-xdcc-mode "erc-xdcc")

(autoload 'erc-xdcc-add-file "erc-xdcc" "\
Add FILE to `erc-xdcc-files'.

\(fn FILE)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "erc-xdcc" "erc-xdcc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from erc-xdcc.el

(register-definition-prefixes "erc-xdcc" '("erc-"))

;;;***

;;;***

(provide 'erc-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; erc-loaddefs.el ends here
