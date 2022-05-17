;;; early-init.el -*- lexical-binding: t; -*-
(push "~/.cache/emacs/eln-cache/" native-comp-eln-load-path)
(defvar ig--file-name-handler-alist file-name-handler-alist)
(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      frame-resize-pixelwise t)
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold 16777216 ; 16mb
		  gc-cons-percentage 0.1
		  file-name-handler-alist ig--file-name-handler-alist)))

