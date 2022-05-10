;;; init.el -*- lexical-binding: t; -*-
(defvar ig--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      frame-resize-pixelwise t
      default-frame-alist '((fullscreen . maximized) (font . "DejaVu Sans Mono-14"))
      vc-follow-symlinks t
      completion-ignore-case t
      tab-always-indent 'complete
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      focus-follow-mouse t
      mouse-autoselect-window t
      disabled-command-function nil
      use-short-answers t
      savehist-file "~/.cache/emacs/savehist"
      recentf-save-file "~/.cache/emacs/recentf"
      x-gtk-show-hidden-files t)
(setq-default inhibit-redisplay t
              inhibit-message t
	      indicate-empty-lines t)
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
(if (not (display-graphic-p))
    (xterm-mouse-mode 1))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'leuven-theme)

(setq org-fontify-whole-heading-line t
      org-startup-with-inline-images t)
(load-theme 'leuven t)

(savehist-mode)
(recentf-mode)
