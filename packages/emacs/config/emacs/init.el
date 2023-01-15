;;; init.el --- Init code -*- lexical-binding: t; -*-

;;; Commentary:
;; Init config.

;;; Code:

(setq vc-follow-symlinks t
      vc-handled-backends '(Git)
      completion-ignore-case t
      tab-always-indent 'complete
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      inhibit-default-init t
      inhibit-startup-screen t
      initial-scratch-message nil
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
      source-directory "~/aur/emacs-git/src/emacs-git/"
      find-file-visit-truename t
      completion-cycle-threshold nil
      sentence-end-double-space nil
      byte-compile-docstring-max-column 999
      require-final-newline t
      save-interprogram-paste-before-kill t
      help-window-select t
      echo-keystrokes 0.01
      eval-expression-print-length nil
      eval-expression-print-level nil
      visual-line-fringe-indicators '(left-curly-arrow nil)
      enable-recursive-minibuffers t
      delete-by-moving-to-trash nil
      confirm-kill-processes nil
      help-window-keep-selected t
      show-paren-context-when-offscreen t)

(custom-set-variables
 '(help-clean-buttons t)
 '(tramp-persistency-file-name (concat ig-cache-dir "tramp"))
 '(project-list-file (concat ig-cache-dir "projects"))
 '(multisession-directory (concat ig-cache-dir "multisession"))
 '(warning-minimum-level :error)
 '(transient-levels-file (concat ig-cache-dir "transient/levels.el"))
 '(transient-values-file (concat ig-cache-dir "transient/values.el"))
 '(transient-history-file (concat ig-cache-dir "transient/history.el"))
 '(find-library-include-other-files nil))

(push ".zwc" completion-ignored-extensions)
(push '("-2d" . ig-open-dired-2pane) command-switch-alist)

(setq-default indicate-empty-lines t
	      read-process-output-max (* 1024 1024))

(global-display-line-numbers-mode)

(defvar generated-autoload-file)
(push (file-name-directory generated-autoload-file) load-path)
(load generated-autoload-file t t t)

(setq frame-title-format
      '((:eval (let ((bname buffer-file-name))
		 (cond
		  (bname
		   (concat (if (buffer-modified-p) "+" "")
			   (file-name-nondirectory bname) ": "
			   (abbreviate-file-name
			    (file-name-directory bname))))
		  ((eq major-mode 'dired-mode) (abbreviate-file-name default-directory))
		  ((eq major-mode 'eshell-mode) (concat "Eshell: " (abbreviate-file-name default-directory)))
		  (t "%b"))))))

(set-default-coding-systems 'utf-8)
(push '(".*" . utf-8) process-coding-system-alist)

(defun ig-hook-kill-emacs ()
  "Kill Emacs hook."
  (when (and (daemonp) (get-buffer "*scratch*"))
    (with-current-buffer "*scratch*"
      (write-file (concat ig-cache-dir "scratch.buf") nil))))
(add-hook 'kill-emacs-hook #'ig-hook-kill-emacs)
(defun ig-hook-after-init ()
  "After init hook."
  (when (get-buffer "*scratch*")
			       (with-current-buffer "*scratch*"
				 (insert-file-contents (concat ig-cache-dir "scratch.buf")))))
(add-hook 'after-init-hook #'ig-hook-after-init)

(run-with-idle-timer 0.1 nil (lambda()
			       (require 'ig-fonts)
			       (require 'ig-packages-load)
			       (require 'ig-packages)))

;; Local Variables:
;; End:
(provide 'init)
;;; init.el ends here
