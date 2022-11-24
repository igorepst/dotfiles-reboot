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
(defun ig-dired-run-proc-async-nohup (&optional attached)
  "Run async process according to ATTACHED."
  (interactive)
  (let ((files (dired-get-marked-files t current-prefix-arg)))
    (if files
	(let ((cmd (dired-guess-default files))
	      (cmd-prefix
	       (unless attached
		 (if (daemonp) "systemd-run --user --collect "
		   "nohup 1>/dev/null 2>&1 "))))
	  (when (listp cmd) (setq cmd (car cmd)))
	  (cond ((null cmd)
		 (setq cmd (read-shell-command "Run async cmd with: " nil 'dired-shell-command-history)))
		((and (= 1 (length (split-string cmd "[[:space:]]"))) (not (executable-find cmd)))
		 (setq cmd (read-shell-command (concat "Run async cmd ('" cmd "' does not exist): ") nil 'dired-shell-command-history))))
	  (when (string-equal cmd "") (setq cmd "xdg-open"))
	  (start-process cmd nil shell-file-name shell-command-switch
			 (concat cmd-prefix cmd " '" (mapconcat #'expand-file-name files "' '") "'")))
      (message "No marked files"))))

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

;;;###autoload
(defun ig-open-dired-2pane (&optional switch first-dir sec-dir split-hor)
  "Open Dired in 2 panes.

SWITCH - is used when calling from cmd.
FIRST-DIR, SEC-DIR - optional directories.
SPLIT-HOR - do the split horizontally."
  (interactive)
  ;; TODO parse switch to options
  (ignore switch)
  (when (called-interactively-p 'any)
    (select-frame (make-frame)))
  (dired (or first-dir "~"))
  (goto-char (point-min))
  (dired-next-dirline 1)
  (if split-hor
      (split-window-horizontally)
    (split-window-vertically))
  (find-file-other-window (or sec-dir "~"))
  (goto-char (point-min))
  (dired-next-dirline 1)
  (other-window -1))

;; https://oremacs.com/2017/03/18/dired-ediff/
;;;###autoload
(defun ig-ediff-files ()
  "Ediff 2 files in Dired."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (error "Use Dired mode"))
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "Second file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "No more than 2 files should be marked"))))

;;;###autoload
(defun ig-find-name-dired (pattern)
  "Search `default-directory' for PATTERN, surrounded by asterisks automatically."
    (interactive
     "sFind-name (filename wildcard): ")
    (find-dired default-directory (concat "-iname \\*" (shell-quote-argument pattern) "\\*")))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-autoload-dired)
;;; ig-autoload-dired.el ends here
