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

(provide 'early-init)
;;; early-init.el ends here

