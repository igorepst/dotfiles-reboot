;;; init.el --- Init code -*- lexical-binding: t; -*-

;;; Commentary:
;; Init config

;;; Code:

(setq default-frame-alist '((fullscreen . maximized) (font . "DejaVuSansMono Nerd Font Mono-14"))
      vc-follow-symlinks t
      completion-ignore-case t
      tab-always-indent 'complete
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-default-init t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'emacs-lisp-mode
      focus-follows-mouse t
      mouse-autoselect-window nil
      disabled-command-function nil
      use-short-answers t
      x-gtk-show-hidden-files t
      tool-bar-style 'both
      auto-save-default nil
      auto-save-list-file-prefix nil
      backup-inhibited t
      create-lockfiles nil
      source-directory "~/.cache/emacs/c-src/emacs"
      find-file-visit-truename t
      completion-cycle-threshold 3
      sentence-end-double-space nil)

(set-face-font 'fixed-pitch-serif "DejaVu Serif-14")
(set-face-font 'variable-pitch "DejaVu Sans-14")

(add-to-list 'completion-ignored-extensions ".zwc")

(setq-default indicate-empty-lines t
	      read-process-output-max (* 1024 1024))
(if (not (display-graphic-p))
    (xterm-mouse-mode 1))

(global-display-line-numbers-mode)

(let ((additional-lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path additional-lisp-dir))

(require 'ig-packages)

(provide 'init)
;;; init.el ends here
