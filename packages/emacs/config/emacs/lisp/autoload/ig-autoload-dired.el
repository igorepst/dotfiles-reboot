;;; ig-autoload-dired.el --- Autoloaded Dired utils  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of autoloaded Dired utils.

;;; Code:

;; https://www.emacswiki.org/emacs/DiredGetFileSize
;;;###autoload
(defun ig-dired-get-size ()
  "Get size of Dired files by `du'."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-schD" "--apparent-size" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]?\\).*total$")
                 (match-string 1))))))

;;;###autoload
(defun ig-dired-run-proc-async-nohup (nohup)
  "Run async process according to NOHUP."
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg))
	 (cmd (dired-guess-default files))
	 (cmd-prefix (if nohup "nohup 1>/dev/null 2>&1 " "")))
    (when (listp cmd) (setq cmd (car cmd)))
    (cond ((null cmd)
	   (setq cmd (read-shell-command "Run async cmd with: " nil 'dired-shell-command-history)))
	  ((and (= 1 (length (split-string cmd "[[:space:]]"))) (not (executable-find cmd)))
	   (setq cmd (read-shell-command (concat "Run async cmd ('" cmd "' does not exist): ") nil 'dired-shell-command-history))))
    (when (equal cmd "") (setq cmd "xdg-open"))
    (start-process cmd nil shell-file-name shell-command-switch
		   (concat cmd-prefix cmd " '" (mapconcat #'expand-file-name files "' '") "'"))))

;;;###autoload
(defun ig-dired-copy-filename-as-kill (&optional arg)
  "Perform `dired-copy-filename-as-kill' swapping no/zero ARG."
  (interactive "P")
  (let ((new-arg
	 (if arg
	     (if (zerop (prefix-numeric-value arg)) nil arg)
	   0)))
    (let ((current-prefix-arg new-arg))
      (call-interactively 'dired-copy-filename-as-kill))))

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
(provide 'ig-autoload-dired)
;;; ig-autoload-dired.el ends here
