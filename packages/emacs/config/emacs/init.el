;;; init.el -*- lexical-binding: t; -*-
(setq default-frame-alist '((fullscreen . maximized) (font . "DejaVuSansMono Nerd Font Mono-14"))
      vc-follow-symlinks t
      completion-ignore-case t
      tab-always-indent 'complete
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      focus-follow-mouse t
      mouse-autoselect-window nil
      disabled-command-function nil
      use-short-answers t
      x-gtk-show-hidden-files t
      org-fontify-whole-heading-line t
      org-startup-with-inline-images t
      tool-bar-style 'both
      auto-save-default nil
      auto-save-list-file-prefix nil
      backup-inhibited t
      create-lockfiles nil
      source-directory "~/.cache/emacs/c-src/emacs"
      find-file-visit-truename t)

(setq-default indicate-empty-lines t)
(if (not (display-graphic-p))
    (xterm-mouse-mode 1))

(global-display-line-numbers-mode)

(let ((additional-lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path additional-lisp-dir))

(require 'ig-packages)
