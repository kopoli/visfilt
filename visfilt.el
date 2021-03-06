;;; visfilt.el --- generic visual filter -*- lexical-binding: t -*-

;; Copyright (C) 2012, 2013 Kalle Kankare

;; Author: Kalle Kankare <kalle.kankare@iki.fi>
;; Created: 15 Jul 2012
;; Package-Requires: ((cl-lib "0.1"))
;; Keywords: display utility, filtering
;; Version: 1.2

;; This file is not part of GNU Emacs.

;; visfilt is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; visfilt is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Visfilt is a generic visual filtering library. The main functionality is to
;; incrementally add to search string with each keypress which will filter the
;; elements on screen.

;; Visfilt is heavily influenced by eassist.el from CEDET.

;;; Change Log:

;; v1.2 2013-10-14
;; - Refactored the whole functionality. Removed visfilt-util.el

;;; Code:

(require 'cl-lib)

(defconst visfilt-default-configuration-alist
  `(:buffer-name "*visfilt*"
    :search-key-list
     "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_"
    :append-key-list ""
    :choose-callback nil
    :search-string-face hi-yellow
    :max-items nil
    :regexp nil
    :header-format-function ,#'visfilt--header-default-format-function
    :display-line-function ,#'car
)
  "Contains default configuration for `visfilt-mode'.

The following kind of keys are supported:

:buffer-name         Name of the visfilt buffer.
:search-key-list     The default key list to use in filtering.
:append-key-list     Extra keys that are appended into the search-key-list.
:choose-callback     Callback that will be run when visfilt returns.
:search-string-face  Face that is set for matching strings.
:max-items           Maximum number of items that are displayed.  If nil, then
                     use (window-height).
:regexp              Is the string interpret as regular expression.

:header-format-function
  Function that displays the header of the buffer.  The prototype
  is the following:

   (lambda (&optional search-str count))

  if both arguments are nil, return the number of rows that the
  header occupies.  Look at `visfilt--header-default-format-function' for
  an example.

:display-line-function
  Function to parse a displayable line from an element of a list
  generated by the `visfilt--choose-filter-function'.  This can be
  used to add formatting to the displayed lines.  Each element
  should be the following format:

    (\"displayable string\" position ...)

  First one must be a string. It is displayed by default on
  screen. The second one depends on the type of filter-function. It
  can be a list index or a buffer position for example.")


;;; filtering features

(defvar visfilt-filter-functions
  '((listp . (visfilt-filter-list))
    (bufferp . (visfilt-filter-buffer))
    (stringp . (visfilt-filter-buffer (lambda (arg) (get-buffer arg)))))
  "List of types that are supported for filtering.

The elements
of the alist are of the following format:

\(type-predicate . (filtering-function conversion-function))

The general functionality is that a variable of type
`type-predicate' is given to the defun `visfilt'.  It is
possibly converted to a proper format using `conversion-function'
if it is not nil.  The argument is then filtered using the
`filtering-function'.

The type-predicate is a type predicate.  Filtering-function is
documented in with `vislist-choose-filter-function'.  The
`conversion-function' gets the argument `elements' of
`visfilt' and returns it in the proper format or nil, if
it is invalid.")

(defun visfilt-filter-list (elements count search-str)
  "Filter a list `ELEMENTS' with the `SEARCH-STR'.
Filters at maximum of `COUNT' elements."
  (cl-loop for e in elements
	   counting e into pos
	   when (and (string-match search-str e) (> (setq count (1- count)) 0))
	   collect (list e pos)))

(defun visfilt-filter-buffer (buffer count search-str)
  "Filter a buffer with the `search-str'."
  (let (ret
	(search-str-p (> (length search-str) 0)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (> count 0)
	(let ((pos (if search-str-p
		       (re-search-forward search-str nil t nil)
		     (point))))
	  (if pos
	      (progn
		(setq ret (cons (list (buffer-substring (point-at-bol) (point-at-eol))
				   (line-number-at-pos)) ret))
		(goto-char (1+ (point-at-eol))))
	    (setq count 0))
	  (setq count (1- count))))
      (nreverse ret))))


;;; internal variables

(defvar visfilt--current-configuration-alist visfilt-default-configuration-alist)

(defvar visfilt--choose-filter-function nil
  "A function with the following arguments (elements count search-str).
Filters the `elements' with the `search-str' at maximum of
`count' times.  `elements' can be anything.  Look for examples at
`visfilt-filter-list' and `visfilt-filter-buffer'.

This is set by defun `visfilt' to a value from
`vislist-filter-function-alist'.")

(defvar visfilt-displayed-elements nil)
(defvar visfilt--search-data nil)
;; (defvar visfilt-search-data-pos nil)
(defvar visfilt--search-string "")


;;; core functionality

(defsubst visfilt--get (ident &optional conf)
  "Get configuration item named `IDENT'.
Get the configuration from `CONF' or from
`visfilt--current-configuration-alist'."
  (plist-get (or conf visfilt--current-configuration-alist) ident))

(defun visfilt--generate-keymap ()
  "Generate the `visfilt-mode' keymap."
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)

    (define-key map (kbd "C-g") 'kill-this-buffer)
    (define-key map (kbd "<RET>") 'visfilt-run-callback)
    (define-key map (kbd "DEL") 'visfilt-search-string-decrement)

    (dolist (k (string-to-list (concat (visfilt--get :search-key-list)
				       (visfilt--get :append-key-list)
				       (if (visfilt--get :regexp)
					   "[]\\.*?^$|=<>`'+"
					 ""))))
      (setq k (char-to-string k))
      (when (string= k " ") (setq k "SPC"))
      (define-key map (read-kbd-macro k)
	'visfilt-search-string-modify))

    map))

(defvar visfilt-mode-map (visfilt--generate-keymap)
  "Keymap for `visfilt-mode'.")

(defun visfilt--header-default-format-function (&optional searchstr count)
  "The default header function.
Displays the `SEARCHSTR' and`COUNT'.  When run without arguments,
returns the number of rows the header will take."
  (if (and (null searchstr) (null count))
      2
    (format "%s (%d): %s\n" (if (visfilt--get :regexp) "Regular expression" "String")
	    count searchstr)))

(defun visfilt-run-callback ()
  "Run the callback that is given to `visfilt'.
This is run when the wanted element is selected."

  (interactive)
  (let ((callback (visfilt--get :choose-callback))
	(elem (nth (- (line-number-at-pos) (funcall (visfilt--get :header-format-function)))
		   visfilt-displayed-elements)))

    (kill-buffer (current-buffer))
    (when (functionp callback)
      (funcall callback elem))))

(defun visfilt--process-search-string ()
  "Selectively `regexp-quote' the `visfilt--search-string'."
  (funcall (if (visfilt--get :regexp) 'concat 'regexp-quote)
	    visfilt--search-string))

(defun visfilt--update ()
  "Update the visfilt buffer with a search string."
  (setq buffer-read-only nil)
  (erase-buffer)
  (let* ((max-items (or (visfilt--get :max-items)
			(- (window-height) 1
			   (funcall (visfilt--get :header-format-function)))))
	 displayed start-point)

    ;; do the filtering and ignore errors
    (condition-case nil
	(setq displayed (funcall visfilt--choose-filter-function
				 visfilt--search-data max-items
				 (visfilt--process-search-string)))
      (invalid-regexp nil))

    ;; insert the header
    (insert (funcall (visfilt--get :header-format-function)
		      visfilt--search-string (length displayed)))
    (setq start-point (point))

    (setq visfilt-displayed-elements displayed)

    ;; insert the filtered elements
    (dolist (line displayed)
      (insert (concat (funcall (visfilt--get :display-line-function) line) "\n")))

    ;; apply the font-face
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (if (> (length visfilt--search-string) 0)
	(hi-lock-face-buffer
	 (visfilt--process-search-string) (visfilt--get :search-string-face)))
    (goto-char start-point)
  (setq buffer-read-only t)))

(defun visfilt-search-string-modify (&optional decrement)
  "Either adds the `last-command-event' as a string to the search
string or if decrement is not nil, then removes one character. In any case
removes the previous search string overlay face."
  (interactive)

  (if (> (length visfilt--search-string) 0)
      (hi-lock-unface-buffer (visfilt--process-search-string)))
  (if decrement
      (let ((len (length visfilt--search-string)))
	(if (> len 0)
	    (setq visfilt--search-string
		  (cl-subseq visfilt--search-string 0
			  (min (- len 1) len)))))
    (setq visfilt--search-string (concat visfilt--search-string
					(char-to-string last-command-event))))
  (visfilt--update))

(defun visfilt-search-string-decrement ()
  "Decrement the visfilt--search-string by one character."
  (interactive)
  (visfilt-search-string-modify t))

(define-derived-mode visfilt-mode fundamental-mode "VisFilt"
  "Visual filtering mode.

\\{visfilt-mode-map}"

  (mapc #'(lambda (x) (make-local-variable x))
	'(visfilt--search-data
	  visfilt--search-string))

  (setq truncate-lines t)
  (hi-lock-mode 1))

(defun visfilt--get-filter (element filter-list)
  "Get the proper filter `ELEMENT' from `FILTER-LIST'.
`FILTER-LIST' should be of similar format as `visfilt-filter-functions'"
  (if filter-list
      (if (funcall (caar filter-list) element)
	  (car filter-list)
	(visfilt--get-filter element (cdr filter-list)))
    nil))


;;; public core functionality

;;;###autoload
(defun visfilt (elements callback &rest config)
  "Choose an element from `ELEMENTS' and return the element through `CALLBACK'.
The `CONFIG' is a configuration which will override
`visfilt-default-configuration-alist'."
  (interactive)

  ;; select the appropriate filter to the given argument
  (let* ((filter-elem (visfilt--get-filter elements visfilt-filter-functions))
	(convert (cl-caddr filter-elem)))
    (when (not filter-elem)
      (error "visfilt: unsupported type for the first argument"))

    (when convert
      (setq elements (funcall convert elements))
      (when (not elements)
	(error "visfilt: conversion to a supported type failed")))

    (setq config (append config `(:choose-callback ,callback)
			 visfilt-default-configuration-alist))
    (switch-to-buffer (get-buffer-create (generate-new-buffer-name
					  (visfilt--get :buffer-name config))) t)

    (visfilt-mode)

    (make-local-variable 'visfilt--choose-filter-function)
    (setq visfilt--choose-filter-function (cadr filter-elem)))

  ;; create the keymap
  (set (make-local-variable 'visfilt--current-configuration-alist) config)
  (set (make-local-variable 'visfilt-mode-map) (visfilt--generate-keymap))
  (use-local-map visfilt-mode-map)

  (make-local-variable 'visfilt-displayed-elements)

  (setq visfilt--search-string "")
  (setq visfilt--search-data elements)

  (visfilt--update))

;;;###autoload
(defmacro visfilt-command-create (name &optional docstring &rest body)
  "Create an interactive function for `visfilt'.

The `NAME' is the symbol that will be created.  In the function
docstring the argument `DOCSTRING' will be the part that the
generated function will filter.  The `BODY' should include a call
to `visfilt'.  The function will have one argument `REGEXP-P'
which should be given to the call to `visfilt'."
  (declare (indent defun) (doc-string 2))
  `(defun ,(intern (symbol-name name)) (&optional regexp-p)
     ,(format (concat "Visually filters %s.\n"
		      "Use \\[universal-argument] to enable regular expression"
		      " matching.") docstring)
     (interactive "P")
     (progn ,@body)))


;;; command functionality

;;;###autoload (autoload 'visfilt-command-buffer-list "visfilt")
(visfilt-command-create visfilt-command-buffer-list
  "the buffer list"
  (visfilt (delq nil
		 (mapcar (lambda (buf)
			   (with-current-buffer buf
			     (let ((name (buffer-name)))
			       (if (string= (substring name 0 1) " ")
				   nil
				 name))))
			 (buffer-list)))
	   (lambda (x) (if x (switch-to-buffer (car x))))
	   :regexp regexp-p :buffer-name "*vf-select-buffer*"))


;;;###autoload (autoload 'visfilt-command-recentf-list "visfilt")
(visfilt-command-create visfilt-command-recentf-list
  "`recentf-list'"
  (when (or (not (boundp 'recentf-list)) (null recentf-list))
    (require 'recentf)
    (recentf-load-list))

  (visfilt recentf-list
	   (lambda (x) (if x (find-file (car x))))
	   :regexp regexp-p :append-key-list ".*/" :buffer-name "*vf-recentf-list*"))


;;;###autoload
(defvar visfilt-command-occur-before-jump-hook nil
  "Hooks to run before jump in `visfilt-command-occur'.")

;;;###autoload
(defvar visfilt-command-occur-after-jump-hook nil
  "Hooks to run after jump in `visfilt-command-occur'.")

(defvar visfilt-command-occur--after-jump-functions nil
  "Functions that are run after `visfilt-command-occur' has jumped to a location.")

(autoload 'org-reveal "org")
(add-hook 'visfilt-command-occur--after-jump-functions
	  #'(lambda () (when (eq major-mode 'org-mode)
			 (org-reveal))))

;;;###autoload (autoload 'visfilt-command-occur "visfilt")
(visfilt-command-create visfilt-command-occur
  "the current buffer `occur'-style"
  (run-hooks 'visfilt-command-occur-before-jump-hook)
  (let ((buffer (current-buffer)))
    (visfilt buffer
	     (lambda (x)
	       (when x
		 (switch-to-buffer buffer)
		 (message "Current buffer %s" (current-buffer))
		 (save-restriction
		   (widen)
		   (goto-char (point-min))
		   (forward-line (1- (cadr x))))
		 (run-hooks 'visfilt-command-occur--after-jump-functions)
		 (run-hooks 'visfilt-command-occur-after-jump-hook)))
	     :buffer-name (format "*vf-occur: %s*" (buffer-name))
	     :display-line-function #'(lambda (elem)
					(format "%4d %s" (cadr elem) (car elem)))
	     :append-key-list "./ "
	     :regexp regexp-p)))


;;;###autoload (autoload 'visfilt-command-find-file-in-tags "visfilt")
(visfilt-command-create visfilt-command-find-file-in-tags
 "the list of files in tags tables"
 (save-excursion
   (let ((tags-file-name nil))
     (visit-tags-table-buffer)

     ;; lecixally bind the tags-file-name, because it might change before
     ;; entering the callback
     (let ((tf tags-file-name))
       (visfilt
	(tags-table-files)
	(lambda (x)
	  (find-file (expand-file-name (car x) (file-name-directory tf))))
	:append-key-list "./"
	:buffer-name "*vf-tags-files*"
	:regexp regexp-p)))))


;;; testing

;; TODO

;; emacs -Q -L $PWD -l visfilt --eval '(setq debug-on-error t stack-trace-on-error t debug-on-quit t)' --eval '(vf-test-buffer-list)'

(provide 'visfilt)

;;; visfilt.el ends here
