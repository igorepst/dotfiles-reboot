;;; ig-autoload-utils.el --- Utils  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provide various utils.

;;; Code:

(defun package-update (name)
  "Update package NAME if a newer version exists."
  (interactive
   (list (completing-read
          "Update package: " (package--updateable-packages) nil t)))
  (let ((package (if (symbolp name)
                     name
                   (intern name))))
    (package-delete (cadr (assq package package-alist)) 'force)
    (package-install package 'dont-select)))

(defun package--updateable-packages ()
  "Initialize the package system to get the list of package symbols for completion."
  (package--archives-initialize)
  (mapcar
   #'car
   (seq-filter
    (lambda (elt)
      (let ((available
             (assq (car elt) package-archive-contents)))
        (and available
             (version-list-<
              (package-desc-priority-version (cadr elt))
              (package-desc-priority-version (cadr available))))))
    package-alist)))

(defun ig-write-selected-packages ()
  "Write selected packages to cache."
  (with-temp-buffer
    (insert-file-contents (concat user-emacs-directory "lisp/ig-packages.el"))
    (keep-lines "ig-selected-packages")
    (flush-lines "makunbound")
    (eval-buffer))
  (let ((additional-lisp-dir (concat user-emacs-directory "lisp")))
    (push additional-lisp-dir load-path))
  (require 'ig-common)
  (write-region
   (concat "(setq package-selected-packages '("  (mapconcat #'symbol-name ig-selected-packages " ") "))\n")
   nil (concat ig-cache-dir "packages/selected-packages") nil))

;;;###autoload
(defun ig-update-local-autoloads ()
  "Update local autoloads."
  (interactive)
  (make-directory (file-name-directory generated-autoload-file) t)
  (let ((inhibit-message t))
    (make-directory-autoloads
     `(,user-emacs-directory ,(concat user-emacs-directory "lisp") ,(concat user-emacs-directory "lisp/autoload"))
     generated-autoload-file))
  (kill-buffer (file-name-nondirectory generated-autoload-file)))

;;;###autoload
(defun ig-update-packages ()
  "Update all packages.

Refresh quickstart as needed automatically in install/delete."
  (interactive)
  (if (version< "29.0" emacs-version)
      (error "Rewrite to use built-in functions!"))
  (ignore-errors (exit-minibuffer))
  (princ "Refreshing package contents\n")
  (require 'ig-packages-load)
  (package-refresh-contents)
  (princ "Done\n")
  (package--archives-initialize)
  ;; Ensure the list is not out of sync
  (princ "Updating selected packages\n")
  (ig-write-selected-packages)
  (load (concat ig-cache-dir "packages/selected-packages") t t nil)
  (princ "Done\n")
  (let ((not-installed (seq-remove #'package-installed-p package-selected-packages)))
    (if (not not-installed)
	(princ "No packages to install\n")
      (princ (format "Installing packages: %s\n" (mapconcat #'symbol-name not-installed " ")))
      (mapc (lambda (p) (package-install p 'dont-select)) not-installed)
      (princ "Done\n")))
  (setq package-selected-packages (copy-tree ig-selected-packages))
  (let ((updateable (package--updateable-packages)))
    (if (not updateable)
        (princ "No packages to update\n")
      (princ (format "Updating packages: %s\n" (mapconcat #'symbol-name updateable " ")))
      (mapc #'package-update updateable)
      (princ "Done\n")))
  (setq package-selected-packages (copy-tree ig-selected-packages))
  (let ((removable (package--removable-packages)))
    (if (not removable)
	(princ "No packages to remove\n")
      (princ (format "Removing packages: %s\n" (mapconcat #'symbol-name removable " ")))
      (mapc (lambda (p)
	      (package-delete (cadr (assq p package-alist)) t))
            removable)
      (princ "Done\n")))
  (princ "Updating local autoloads\n")
  (ig-update-local-autoloads)
  (princ "Done\n"))

(defun ig-byte-recompile-config()
  "Byte-recompile all the config files.

This requires all the selected packages to be available at compile time,
hence is suitable to be called in batch mode only."
  (princ "Byte-recompile the config\n")
  (load (concat ig-cache-dir "packages/selected-packages") t t nil)
  (mapc #'require package-selected-packages)
  (byte-recompile-directory user-emacs-directory 0 t)
  (princ "Done\n"))

;;;###autoload
(defun ig-describe-symbol ()
  "Describe symbol at point."
  (interactive)
  (let ((thing (symbol-at-point)))
    (if thing (describe-symbol thing) (call-interactively #'describe-symbol))))

;;;###autoload
(defun ig-kitty-scrollback (input-line cur-x cur-y)
  "Open and process Kitty terminal scrollback.

INPUT-LINE - the number of lines a pager should scroll.
CUR-X and CUR-Y - cursor X and Y."
  (require 'ig-packages)
  (let ((buf (find-file-noselect (concat "/tmp/" ig-kitty-scrollback-file) t)))
    (pop-to-buffer-same-window buf))
  (revert-buffer t t)
  (goto-char (point-min))
  (while (re-search-forward "]8;;file:[^\\\\]*\\\\\\|]8;;\\\\\\\\|\\[m]133;[AC]\\\\" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\[39m \\[4m" nil t)
    (replace-match " "))
  (font-lock-mode -1)
  (xterm-color-colorize-buffer)
  (save-buffer)
  (goto-char (point-min))
  (forward-line (+ input-line cur-y))
  (beginning-of-line)
  (forward-char (- cur-x 1))
  (recenter))

;; https://emacs.stackexchange.com/a/8177/2477
;;;###autoload
(defun presorted-completion-table (completions)
  "Disable sorting of COMPLETIONS."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action action completions string pred))))

;; http://www.howardism.org/Technical/Emacs/alt-completing-read.html
;;;###autoload
(defun alt-completing-read (prompt collection &optional nosort def)
  "Call `completing-read' but return the value from COLLECTION, using the PROMPT."
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))
    (let* ((choice
            (completing-read prompt
			     (if nosort
				 (presorted-completion-table collection)
			       collection)))
           (results (cond
                     ((hash-table-p collection) (gethash choice collection))
                     ((assoc-list-p collection) (alist-get choice collection def nil 'equal))
                     (t                         choice))))
      results)))

;; TODO it freezes sometimes
;;;###autoload
(defun ig-restart-emacs-daemon()
  "Restart Emacs daemon."
  (interactive)
  (save-some-buffers t t)
  (recentf-save-list)
  (savehist-save)
  (start-process "Remd" nil shell-file-name shell-command-switch "systemctl --user restart emacs"))

;;;###autoload
(defun ig-read-pathmarks-dwim()
  "Read Pathmarks file from Zsh."
  (interactive)
  (let ((choice
	 (with-temp-buffer
	   (insert-file-contents "~/.zsh/volatile/pathmarks")
	   (goto-char (point-min))
	   (let ((cand '()))
	     (while (not (eobp))
	       (let* ((line (buffer-substring (point)
					      (progn (forward-line 1) (point))))
		      (spl (split-string line ":" t "\\s-+"))
		      (spath (abbreviate-file-name (cadr spl)))
		      (fpath (concat (car spl) " -> " spath)))
		 (push `(,fpath . ,spath) cand)))
	     (push `(,(concat "emacs -> " user-emacs-directory) . ,user-emacs-directory) cand)
	     (alt-completing-read "Choose pathmark: " (nreverse cand) t)))))
    (when choice
      (pcase major-mode
	('eshell-mode
	 (eshell-interactive-print (concat "cd " choice))
	 (eshell/cd choice)
	 (eshell-send-input))
	('dired-mode
	 (dired choice))
	(_
	 (kill-new choice)
	 (message (concat "Path '" choice "' is copied")))))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-autoload-utils)
;;; ig-autoload-utils.el ends here
