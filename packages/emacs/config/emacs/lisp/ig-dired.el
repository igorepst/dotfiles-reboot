;;; ig-dired.el --- Dired configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of Dired.

;;; Code:

(defconst ig-ls-switches "-Alh --group-directories-first --time-style \"+%d-%m-%Y %H:%M\"" "'ls' switches.")

(setq dired-use-ls-dired t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-listing-switches (purecopy ig-ls-switches))

(defun ig-dired-sort-helper (variant &optional reverse)
  "Sort Dired by VARIANT, possibly in REVERSE order.
The sorting mode will be used from now on."
  (let ((switches (concat (purecopy ig-ls-switches) " " variant)))
    (when reverse (setq switches (concat "--reverse " switches)))
    (setq dired-listing-switches switches)
    (when (eq major-mode 'dired-mode)
      (dired-sort-other switches))))

;;;###autoload
(defun ig-dired-sort (sort-order)
  "Sort Dired according to SORT-ORDER."
  (interactive (list (alt-completing-read "Sort order: "
					  '(("Name" . ("-v" . nil))
					    ("Name reversed" . ("-v" . t))
					    ("Time" . ("-t" . nil))
					    ("Time reversed" . ("-t" . t))
					    ("Size" . ("-S" . nil))
					    ("Size reversed" . ("-S" . t))
					    ("Ext" . ("-X" . nil))
					    ("Ext reversed" . ("-X" . t))))))
  (ig-dired-sort-helper (car sort-order) (cdr sort-order)))

(defun ig-dired-sort-set-mode-line (_args)
  "Override mode name."
  (setq mode-name
	(concat
	 (cond ((string-match-p
		 "-v$" dired-actual-switches)
		"Dir name")
	       ((string-match-p
		 "-t$" dired-actual-switches)
		"Dir time")
	       ((string-match-p
		 "-S$" dired-actual-switches)
		"Dir size")
	       ((string-match-p
		 "-X$" dired-actual-switches)
		"Dir ext")
	       (t
		(concat "Dired " dired-actual-switches)))
	 (if (string-match-p "^--reverse" dired-actual-switches)
	     " ↓" " ↑")))
  (force-mode-line-update))
(advice-add 'dired-sort-set-mode-line :around #'ig-dired-sort-set-mode-line)

(ig-dired-sort-helper "-v" nil)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-dired)
;;; ig-dired.el ends here
