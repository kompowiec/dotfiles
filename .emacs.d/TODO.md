# stop Error (use-package): evil/ catch: Can’t make ‘nil’ obsolete, did you forget a quote mark?
The error message you're encountering with `use-package` for the `evil` package in GNU Emacs suggests that there's an issue with the configuration of `evil` where a value that should be quoted is not. This often happens when a symbol or keyword is expected, but `nil` is found instead, possibly due to a missing quote mark.

Here's what you can do to troubleshoot this issue:

1. Check for any missing quote marks in your configuration that might be causing `nil` to be evaluated incorrectly.
2. Ensure that the configuration syntax for `use-package` is correct. For example, if you're setting a variable to `nil`, it should be quoted like this: `(setq some-variable 'nil)`.
3. Review the `use-package` declaration for `evil` and look for any unquoted symbols or keywords.

If you're using a configuration like this:

```elisp
(use-package evil
  :init
  (setq evil-want-keybinding nil))
```

Make sure that `nil` is not being treated as a variable that needs to be made obsolete. If `nil` is the intended value, it should be quoted or the line should be written in a way that doesn't trigger the obsolescence check.

Additionally, there was a known issue reported on GitHub regarding a similar error, which might be related to your problem. It could be beneficial to check the discussions and solutions provided there¹.

Remember to always back up your configuration before making changes, so you can revert if something goes wrong. If you provide more details or the specific part of your configuration that's causing the error, I could offer more targeted advice.

Source: Conversation with Bing, 3/24/2024
(1) Can’t make ‘nil’ obsolete; did you forget a quote mark? #1839. https://github.com/emacs-evil/evil/issues/1839.
(2) Troubleshooting Options (use-package User Manual). https://www.gnu.org/software//emacs/manual/html_node/use-package/Troubleshooting-Options.html.
(3) Troubleshooting Options (use-package User Manual). https://www.gnu.org/software/emacs/manual/html_node/use-package/Troubleshooting-Options.html.
(4) Getting a no match when trying to install evil from emacs (M-x package .... https://stackoverflow.com/questions/71877781/getting-a-no-match-when-trying-to-install-evil-from-emacs-m-x-package-install-r.
(5) error: Package `use-package-' is unavailable - Emacs Stack Exchange. https://emacs.stackexchange.com/questions/39250/error-package-use-package-is-unavailable.

# error: Eager macro-expansion failure: (error "Shortdoc f function ‘f-older -p’: bad keyword ‘:noeval’")

It seems like you're encountering an eager macro-expansion failure in GNU Emacs. This error typically occurs when Emacs tries to evaluate something at macro-expansion time that it shouldn't. To resolve this, ensure that you're not evaluating the file name expansion or using `eval` at expansion time. The macro should only place the value of the directory inside the returned expression during its expansion.

Here's a general approach to avoid such errors:

1. Remove any `eval` calls from your macro definitions.
2. Use the comma operator `,` to insert the expression into the macro expansion properly.

For example, if you have a macro like this:

```elisp
(defmacro with-current-directory (directory &rest body)
  `(let ((default-directory ,(file-name-as-directory (expand-file-name (eval directory)))))
     ,@body))
```

You should change it to:

```elisp
(defmacro with-current-directory (directory &rest body)
  `(let ((default-directory (file-name-as-directory (expand-file-name ,directory))))
     ,@body))
```

Notice the removal of `eval` and the placement of the comma before `directory`. This ensures that `directory` is treated as a Lisp expression that evaluates to the value, rather than being evaluated at macro-expansion time¹.

If you're still facing issues, it might be helpful to look at the context in which the macro is used and check for any variables or expressions that might be causing the problem. If necessary, you can also share more details or code snippets for further assistance.

Source: Conversation with Bing, 3/24/2024
(1) How do I avoid eager macro expansion failures in emacs macros?. https://stackoverflow.com/questions/25097816/how-do-i-avoid-eager-macro-expansion-failures-in-emacs-macros.
(2) Transition to emacs 29.1, "Eager macro-expansion failure" error at startup. https://emacs.stackexchange.com/questions/79253/transition-to-emacs-29-1-eager-macro-expansion-failure-error-at-startup.
(3) error - "Eager macro-expansion failure" message when starting Emacs 28. .... https://emacs.stackexchange.com/questions/74421/eager-macro-expansion-failure-message-when-starting-emacs-28-2-from-the-mac-te.
(4) eager macro-expansion failure wrong number of arguments with emacs 28 .... https://github.com/emacs-evil/evil/issues/1403.
(5) undefined. http://melpa.org/packages/.
(6) undefined. http://elpa.gnu.org/packages/.
(7) undefined. http://melpa.milkbox.net/packages/.
