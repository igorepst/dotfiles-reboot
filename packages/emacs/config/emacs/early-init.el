;;; early-init.el --- Early init code -*- lexical-binding: t; -*-

;;; Commentary:
;; Early init config

;;; Code:

(push "~/.cache/emacs/eln-cache/" native-comp-eln-load-path)
(defvar ig--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t)
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 134217728 ; 128 Mb
		  gc-cons-percentage 0.1
		  file-name-handler-alist ig--file-name-handler-alist)
	    (makunbound 'ig--file-name-handler-alist)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-enable-at-startup t
      package-user-dir "~/.cache/emacs/packages/install"
      package-native-compile t
      package-quickstart t
      package-quickstart-file "~/.cache/emacs/packages/package-quickstart.el"
      package-gnupghome-dir "~/.cache/emacs/packages/gnupg"
      package-selected-packages '(lua-mode vertico orderless marginalia
					   consult consult-flycheck corfu cape
					   embark embark-consult flycheck
					   lsp-mode lsp-ui lsp-pyright
					   consult-lsp esup))

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

(defun ig-update-packages ()
  "Update all packages."
  (interactive)
  (if (version< "29.0" emacs-version)
      (error "Rewrite to use built-in functions!"))
  (ignore-errors (exit-minibuffer))
  (princ "Refreshing package contents\n")
  (package-refresh-contents)
  (princ "Done\n")
  (let ((updateable (package--updateable-packages)))
    (if (not updateable)
        (princ "No packages to update\n")
      (princ (format "Updating packages: %s\n" (mapconcat #'symbol-name updateable " ")))
      (mapc #'package-update updateable)
      (princ "Done\n")))
  (let ((removable (package--removable-packages)))
    (if (not removable)
	(princ "Nothing to remove\n")
      (princ (format "Removing packages: %s\n" (mapconcat #'symbol-name removable " ")))
      (mapc (lambda (p)
              (package-delete (cadr (assq p package-alist)) t))
            removable)
      (princ "Done\n"))))

(provide 'early-init)
;;; early-init.el ends here

