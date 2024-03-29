;;; early-init.el --- Early init code -*- lexical-binding: t; -*-

;;; Commentary:
;; Early init config

;;; Code:

(push (concat user-emacs-directory "lisp") load-path)
(require 'ig-common)
(defvar ig--file-name-handler-alist file-name-handler-alist)
(defvar generated-autoload-file)
(setq default-frame-alist `((font . "DejaVuSansMono Nerd Font Mono-14") (cursor-color . ,ig-color-orange))
      native-comp-eln-load-path `(,(concat ig-cache-dir "eln-cache")
				 ,(car (last native-comp-eln-load-path)))
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0
      file-name-handler-alist nil
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      generated-autoload-file (concat ig-cache-dir "local-autoloads/loaddefs.el")
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
;; End:
(provide 'early-init)
;;; early-init.el ends here

