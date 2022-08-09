;;; ig-autoload-edit.el --- Editing functions

;;; Commentary:
;; Various helpers for editing.

;;; Code:

;;;###autoload
(defun ig-get-bounds-lines-rect (&optional buf)
  "Get bounds of region, line or buffer for BUF."
  (cond ((use-region-p)
	 (let ((start (point)) (end (mark)))
	   (when (> start end)
	     (setq start (prog1 end (setq end start)))
	     (goto-char start))
	   (setq start (line-beginning-position))
	   (goto-char end)
	   (setq end (line-end-position))
	   (cons start end)))
	(buf (cons (point-min) (point-max)))
	(t (cons (line-beginning-position) (line-end-position)))))

;; https://github.com/bbatsov/crux/blob/master/crux.el
;;;###autoload
(defun ig-duplicate-current-line-or-region (arg)
  "Duplicate the current line or all lines of the region ARG times."
  (interactive "*p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (ig-get-bounds-lines-rect))
               (region (buffer-substring-no-properties beg end)))
    (dotimes (_i arg)
      (goto-char end)
      (newline)
      (insert region)
      (setq end (point)))
    (goto-char (+ origin (* (length region) arg) arg))))

;;;###autoload
(defun ig-copy-current-line-or-region ()
  "Copy current line or all lines of the region."
  (interactive)
  (let ((bounds (ig-get-bounds-lines-rect)))
    (kill-ring-save (car bounds) (cdr bounds))))

;;;###autoload
(defun ig-kill-current-line-or-region ()
  "Kill current line or all lines of the region."
  (interactive "*")
  (let ((bounds (ig-get-bounds-lines-rect)))
    (kill-region (car bounds) (cdr bounds))))

;;;###autoload
(defun ig-select-current-line-or-region ()
  "Select current line or all lines of the region."
  (interactive)
  (let ((bounds (ig-get-bounds-lines-rect)))
    (set-mark (car bounds))
    (goto-char (cdr bounds))
    (activate-mark)))

;;;###autoload
(defun ig-delete-matching-lines (regexp)
  "Delete lines matching REGEXP in region or buffer."
  (interactive "*MFlush lines containing match for regexp: ")
  (let ((bounds (ig-get-bounds-lines-rect t)))
    (flush-lines regexp (car bounds) (cdr bounds) t)))

;;;###autoload
(defun ig-delete-non-matching-lines (regexp)
  "Keep only lines matching REGEXP in region or buffer."
  (interactive "*MKeep lines containing match for regexp: ")
  (let ((bounds (ig-get-bounds-lines-rect t)))
    (keep-lines regexp (car bounds) (cdr bounds) t)))

;;;###autoload
(defun ig-sort-lines (&optional reverse)
  "Sort lines in region or buffer.
Optional argument REVERSE - whether to reverse the sort."
  (interactive "*")
  (let ((bounds (ig-get-bounds-lines-rect t)))
    (sort-lines reverse (car bounds) (cdr bounds))))

;;;###autoload
(defun ig-reverse-region ()
  "Reverse lines in region or buffer."
  (interactive "*")
  (let ((bounds (ig-get-bounds-lines-rect t)))
    (reverse-region (car bounds) (cdr bounds))))

;;;###autoload
(defun ig-font-lock-log-file ()
  "Highlight severity keywords in log files."
  (font-lock-add-keywords nil '(("\\<\\(ERROR\\|FATAL\\)\\>" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(WARN\\)\\>" 1 font-lock-keyword-face t)))
  (font-lock-add-keywords nil '(("\\<\\(INFO\\)\\>" 1 font-lock-function-name-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DEBUG\\|TRACE\\)\\>" 1 font-lock-constant-face t))))

;;;###autoload
(defun font-lock-comment-annotations ()
  "Highlight comment annotations."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|NOSONAR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

;; Open file with sudo. Warn on root
;; http://stackoverflow.com/a/18951887/407953
;;;###autoload
(defun ig-find-alternative-file-with-sudo ()
  "Open the file with 'sudo'."
  (interactive)
  (let ((bname (expand-file-name (or buffer-file-name
                                     default-directory)))
        (pt (point)))
    (setq bname (or (file-remote-p bname 'localname)
                    (concat "/sudo::" bname)))
    (cl-flet ((server-buffer-done
               (buffer &optional for-killing)
               nil))
      (find-alternate-file bname))
    (goto-char pt)))

;;;###autoload
(defun ig-find-file-root-header-warning ()
  "Display a warning in header line of the current buffer.
This function is suitable to add to `find-file-hook'."
  (when (string-equal
         (file-remote-p (or buffer-file-name default-directory) 'user)
         "root")
    (let* ((warning "WARNING: EDITING AS ROOT!")
	   (space (- (+ 4 (window-width)) (length warning)))
           (bracket (make-string (/ space 2) ? ))
           (warning (concat bracket warning bracket)))
      (setq header-line-format
            (propertize  warning 'face '(:foreground "white" :background "red3"))))))

(provide 'ig-autoload-edit)
;;; ig-autoload-edit.el ends here
