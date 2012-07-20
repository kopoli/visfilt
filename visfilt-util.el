
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
  (let ((visfilt-search-key-list (concat visfilt-search-key-list "/"))
	(visfilt-buffer-name "*vf-select-buffer*"))
    (visfilt-choose (mapcar (lambda (buf) 
			      (with-current-buffer buf
				(buffer-name)))  
			    (buffer-list))
		    (lambda (x) (if (not (null x)) 
				    (switch-to-buffer (car x)))))))

(defun vf-recent-file-list ()
  "Uses visfilt to select from recent files"
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
