Emacs Lisp is the scripting language used in the Emacs editor. Here's a cheat sheet with some useful Emacs Lisp commands:

Comments
;; This is a comment in Emacs Lisp

Variables
(defvar variable-name value "Docstring") ; Define a variable with initial value
(setq variable-name value) ; Set a variable value
(let ((variable-name value)) body) ; Create a local variable

Data Types
3.1. Numbers:
(+ 2 3) ; Addition
(- 5 2) ; Subtraction
(* 2 4) ; Multiplication
(/ 8 2) ; Division
(% 9 4) ; Modulo

3.2. Strings:
(concat "Hello" " World") ; Concatenation
(substring "Hello World" 0 5) ; Substring

3.3. Lists:
'(1 2 3) ; A quoted list
(list 1 2 3) ; A list with the list function
(car '(1 2 3)) ; Get the first element of a list
(cdr '(1 2 3)) ; Get the rest of a list
(cons 0 '(1 2 3)) ; Add an element to the beginning of a list

Control Structures
4.1. Conditionals:
(if condition then-body else-body)
(cond (condition1 then-body1) (condition2 then-body2) (t else-body))
4.2. Loops:
(while condition body)
(dolist (var list) body)
(mapcar #'function list) ; Apply a function to each element of a list

Functions
(defun function-name (arg1 arg2) "Docstring" body)
(defun square (x) (* x x))
(defun hello (name) (message "Hello, %s!" name))
(funcall #'function arg1 arg2) ; Call a function with arguments
.apply #’function args; Call a function with arguments

Buffers and Files
(buffer-name) ; Get the name of the current buffer
(buffer-file-name) ; Get the file name of the current buffer
(find-file "filename") ; Open a file
(save-buffer) ; Save the current buffer
(kill-buffer) ; Close the current buffer

Miscellaneous
(message "Hello, World!") ; Print a message in the echo area
(read-from-minibuffer "Prompt:") ; Read a string from the user
(sit-for seconds) ; Wait for a number of seconds
(sleep-for seconds) ; Sleep for a number of seconds

These are some basic commands in Emacs Lisp, but there are many more. For further information, consult the Emacs Lisp Reference Manual or the Emacs Lisp Intro.
