;;; ig-utils.el --- Utils  -*- lexical-binding: t; -*-

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

;;;###autoload
(defun ig-update-local-autoloads ()
  "Update local autoloads."
  (interactive)
  (make-directory (file-name-directory generated-autoload-file) t)
  (let ((inhibit-message t))
    (make-directory-autoloads `(,user-emacs-directory ,(expand-file-name "lisp" user-emacs-directory)) generated-autoload-file))
  (kill-buffer (file-name-nondirectory generated-autoload-file)))

;;;###autoload
(defun ig-update-packages ()
  "Update all packages."
  (interactive)
  (if (version< "29.0" emacs-version)
      (error "Rewrite to use built-in functions!"))
  (require 'package)
  (ignore-errors (exit-minibuffer))
  (princ "Refreshing package contents\n")
  (package-refresh-contents)
  (princ "Done\n")
  (package--archives-initialize)
  (let ((not-installed (seq-remove #'package-installed-p package-selected-packages)))
    (if (not not-installed)
	(princ "No packages to install\n")
      (princ (format "Installing packages: %s\n" (mapconcat #'symbol-name not-installed " ")))
      (mapc (lambda (p) (package-install p 'dont-select)) not-installed)
      (princ "Done\n")))
  (let ((updateable (package--updateable-packages)))
    (if (not updateable)
        (princ "No packages to update\n")
      (princ (format "Updating packages: %s\n" (mapconcat #'symbol-name updateable " ")))
      (mapc #'package-update updateable)
      (princ "Done\n")))
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

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'ig-utils)
;;; ig-utils.el ends here
