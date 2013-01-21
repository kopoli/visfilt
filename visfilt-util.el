;;; visfilt-util.el --- utilities using visfilt

;; Copyright (C) 2012 Kalle Kankare

;; Author: Kalle Kankare <kalle.kankare@iki.fi>
;; Maintainer: Kalle Kankare <kalle.kankare@iki.fi>
;; Created: 20 Jul 2012

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file includes special functionality around the generic visfilt
;; core. The following functions are included:

;; - vf-find-file-in-tags -- Filters the list of files found from TAGS files.
;; - vf-buffer-list -- Filters the buffer list.
;; - vf-recent-file-list -- Filters the recentf-list.
;; - vf-occur-jump -- Filters the current buffer in similar manner as occur.

;;; Code:

(require 'recentf)
(require 'visfilt)

(defun vf-find-file-in-tags ()
  "Uses visfilt to select file in TAGS table"
  (interactive)
  (save-excursion
    (let ((tags-file-name nil)
	  (visfilt-search-key-list (concat visfilt-search-key-list "./"))
	  (visfilt-buffer-name "*vf-tags-files*"))
      (visit-tags-table-buffer)
      ;; make this variable bound here, because it can change when lambda is called
      (lexical-let ((tags-file-name tags-file-name))
	(visfilt-choose
	 (tags-table-files)
	 (lambda (x)
	   (find-file (expand-file-name (car x) (file-name-directory tags-file-name)))))))))

(defun vf-buffer-list ()
  "Uses visfilt to select buffer"
  (interactive)
  (let ((visfilt-search-key-list (concat visfilt-search-key-list ".*/"))
	(visfilt-buffer-name "*vf-select-buffer*"))
    (visfilt-choose (delq nil (mapcar (lambda (buf)
				    (with-current-buffer buf
				      (let ((name (buffer-name)))
					(if (string= (substring name 0 1) " ")
					    nil
					  name))))
				      (buffer-list)))
		    (lambda (x) (if (not (null x))
				    (switch-to-buffer (car x)))))))

(defun vf-recent-file-list ()
  "Uses visfilt to select from the list of recent files"
  (interactive)

  (let ((visfilt-search-key-list (concat visfilt-search-key-list "./"))
	(visfilt-buffer-name "*vf-recentf*"))
    (visfilt-choose recentf-list
		    (lambda (x) (if (not (null x))
				    (find-file (car x)))))))


(defvar vf-occur-jump-arrive-hook nil
  "Hook is called when `vf-occur-jump' has jumped at the given
position."  )

(defun vf-occur-jump ()
  "Uses visfilt to jump around the current buffer."
  (interactive)
  (let ((visfilt-buffer-name "*vf-occur*")
	(visfilt-search-key-list (concat visfilt-search-key-list "./ "))
	(visfilt-display-line-function
	 #'(lambda (elem)
	    (format "%4d %s" (cadr elem) (car elem)))))
    (visfilt-choose
     (current-buffer)
     (lambda (x) (when x
		   (save-restriction
		     (widen)
		     (goto-char (point-min))
		     (forward-line (1- (cadr x)))
		     (run-hooks 'vf-occur-jump-arrive-hook)))))))

(eval-after-load 'org
  (add-hook 'vf-occur-jump-arrive-hook
	    (lambda ()
	      ;; inform org-mode to actually show the place
	      (if (eq major-mode 'org-mode)
		  (org-show-context)))))

(provide 'visfilt-util)

;;; visfilt-util.el ends here
