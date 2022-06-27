;;; early-init.el --- Early init code -*- lexical-binding: t; -*-

;;; Commentary:
;; Early init config

;;; Code:

(let ((additional-lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (push additional-lisp-dir load-path))

(require 'ig-common)
(push (expand-file-name "eln-cache" ig-cache-dir) native-comp-eln-load-path)
(defvar ig--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      generated-autoload-file (expand-file-name "local-autoloads/loaddefs.el" ig-cache-dir))
(setq-default inhibit-redisplay t
              inhibit-message t
	      load-prefer-newer t)
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
      package-user-dir (expand-file-name "packages/install" ig-cache-dir)
      package-native-compile t
      package-quickstart t
      package-quickstart-file (expand-file-name "packages/package-quickstart.el" ig-cache-dir)
      package-gnupghome-dir (expand-file-name "packages/gnupg" ig-cache-dir)
      package-selected-packages '(lua-mode vertico orderless marginalia
					   consult consult-flycheck corfu cape
					   embark embark-consult flycheck
					   lsp-mode lsp-ui lsp-pyright
					   consult-lsp kind-icon))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'early-init)
;;; early-init.el ends here

