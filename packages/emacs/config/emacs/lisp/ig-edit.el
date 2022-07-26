;;; ig-edit.el --- Editing functions

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
  (interactive "p")
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
  (let ((cell (ig-get-bounds-lines-rect)))
    (kill-ring-save (car cell) (cdr cell))))

;;;###autoload
(defun ig-kill-current-line-or-region ()
  "Kill current line or all lines of the region."
  (interactive)
  (let ((cell (ig-get-bounds-lines-rect)))
    (kill-region (car cell) (cdr cell))))

;;;###autoload
(defun ig-select-current-line-or-region ()
  "Select current line or all lines of the region."
  (interactive)
  (let ((cell (ig-get-bounds-lines-rect)))
    (set-mark (car cell))
    (goto-char (cdr cell))
    (activate-mark)))

(provide 'ig-edit)
;;; ig-edit.el ends here
