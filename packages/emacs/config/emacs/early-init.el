;;; early-init.el --- Early init code -*- lexical-binding: t; -*-

;;; Commentary:
;; Early init config

;;; Code:

(push (expand-file-name "lisp" user-emacs-directory) load-path)
(require 'ig-common)
(push (expand-file-name "eln-cache" ig-cache-dir) native-comp-eln-load-path)
(defvar ig--file-name-handler-alist file-name-handler-alist)
(setq default-frame-alist `((font . "DejaVuSansMono Nerd Font Mono-14") (cursor-color . ,ig-color-orange))
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      generated-autoload-file (expand-file-name "local-autoloads/loaddefs.el" ig-cache-dir)
      package-enable-at-startup nil)
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
	    (setq gc-cons-threshold 33554432 ; 32 Mb
		  gc-cons-percentage 0.1
		  file-name-handler-alist ig--file-name-handler-alist)
	    (makunbound 'ig--file-name-handler-alist)))

(tool-bar-mode -1)
(blink-cursor-mode -1)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'early-init)
;;; early-init.el ends here

