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
    (insert-file-contents (expand-file-name "lisp/ig-packages.el" user-emacs-directory))
    (keep-lines "ig-selected-packages")
    (flush-lines "makunbound")
    (eval-buffer))
  (let ((additional-lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (push additional-lisp-dir load-path))
  (require 'ig-common)
  (write-region
   (concat "(setq package-selected-packages '("  (mapconcat #'symbol-name ig-selected-packages " ") "))\n")
   nil (expand-file-name "packages/selected-packages" ig-cache-dir) nil))

;;;###autoload
(defun ig-update-local-autoloads ()
  "Update local autoloads."
  (interactive)
  (make-directory (file-name-directory generated-autoload-file) t)
  (let ((inhibit-message t))
    (make-directory-autoloads
     `(,user-emacs-directory ,(expand-file-name "lisp" user-emacs-directory) ,(expand-file-name "lisp/autoload" user-emacs-directory))
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
  (require 'package)
  (package-refresh-contents)
  (princ "Done\n")
  (package--archives-initialize)
  ;; Ensure the list is not out of sync
  (princ "Updating selected packages\n")
  (ig-write-selected-packages)
  (load (expand-file-name "packages/selected-packages" ig-cache-dir) t t nil)
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
  (let ((buf (find-file-noselect (expand-file-name ig-kitty-scrollback-file "/tmp") t)))
    (switch-to-buffer buf))
  (revert-buffer t t)
  (goto-char (point-min))
  (while (re-search-forward "]8;;file:[^\\\\]*\\\\\\|]8;;\\\\\\\\|\\[m]133;[AC]\\\\" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\[39m \\[4m" nil t)
    (replace-match " "))
  ;; Convert color to TextProperties. Copy without formatting
  ;; See also https://www.emacswiki.org/emacs/TtyFormat
  ;; (require 'ansi-color)
  ;; (ansi-color-apply-on-region (point-min) (point-max))
  ;; (setq xterm-color-names `[,ig-color-black ,ig-color-red ,ig-color-green ,ig-color-yellow ,ig-color-blue ,ig-color-magenta ,ig-color-cyan ,ig-color-white]
  ;; 	xterm-color-names-bright `[,ig-color-bright-black ,ig-color-bright-red ,ig-color-bright-green ,ig-color-bright-yellow
  ;; 							  ,ig-color-bright-blue ,ig-color-bright-magenta ,ig-color-bright-cyan ,ig-color-bright-white])
  (font-lock-mode -1)
  ;; TODO fix colors. Note 'ls' output
  (xterm-color-colorize-buffer)
  (save-buffer)
  (goto-char (point-min))
  (forward-line (+ input-line cur-y))
  (beginning-of-line)
  (forward-char (- cur-x 1))
  (recenter)
  ;; TODO doesn't work
  (read-only-mode 1))

;; http://www.howardism.org/Technical/Emacs/alt-completing-read.html
;;;###autoload
(defun alt-completing-read (prompt collection &optional nosort def)
  "Call `completing-read' but return the value from COLLECTION, using the PROMPT."
  (cl-flet ((assoc-list-p (obj) (and (listp obj) (consp (car obj)))))
    (let* ((choice
            (completing-read prompt
			     (if nosort
				 (lambda (string predicate action)
				   (if (eq action 'metadata)
				       `(metadata (display-sort-function . ,#'identity)
						  (cycle-sort-function . ,#'identity))
				     (complete-with-action action collection string predicate)))
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
  (start-process "Remd" nil shell-file-name shell-command-switch "systemctl --user restart emacs"))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-autoload-utils)
;;; ig-autoload-utils.el ends here
