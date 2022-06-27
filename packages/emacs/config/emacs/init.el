;;; init.el --- Init code -*- lexical-binding: t; -*-

;;; Commentary:
;; Init config.

;;; Code:

(setq vc-follow-symlinks t
      completion-ignore-case t
      tab-always-indent 'complete
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-default-init t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode
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
      source-directory (expand-file-name "c-src/emacs" ig-cache-dir)
      find-file-visit-truename t
      completion-cycle-threshold 3
      sentence-end-double-space nil
      byte-compile-docstring-max-column 999)

(add-to-list 'completion-ignored-extensions ".zwc")

(setq-default indicate-empty-lines t
	      read-process-output-max (* 1024 1024))

(global-display-line-numbers-mode)

(push (file-name-directory generated-autoload-file) load-path)
(load generated-autoload-file t t t)

(load-theme 'adwaita t)

(require 'ig-fonts)
(require 'ig-packages)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'init)
;;; init.el ends here
