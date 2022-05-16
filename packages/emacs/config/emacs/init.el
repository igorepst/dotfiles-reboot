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
      mouse-autoselect-window t
      disabled-command-function nil
      use-short-answers t
      x-gtk-show-hidden-files t
      org-fontify-whole-heading-line t
      org-startup-with-inline-images t
      tool-bar-style 'both
      auto-save-default nil
      make-backup-files nil)

(setq-default indicate-empty-lines t)
(if (not (display-graphic-p))
    (xterm-mouse-mode 1))

(setq straight-check-for-modifications nil)
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
(straight-use-package 'use-package)

(use-package lua-mode
  :straight t
  :defer t
  :mode ("\\.lua\\'" . lua-mode))

(use-package eglot
  :straight t
  :commands (eglot eglot-ensure)
  :hook ((lua-mode . eglot-ensure) (sh-mode . eglot-ensure))
  :config
  (let* ((ig--sumneko-root-path "~/.cache/lspServers/lua/sumneko-lua/extension/server")
	 (ig--sumneko-bin (expand-file-name "bin/lua-language-server" ig--sumneko-root-path))
	 (ig--sumneko-main (expand-file-name "main.lua" ig--sumneko-root-path))
	 (ig--sumneko-settings (concat "--configpath=" (expand-file-name "external/sumnekoSettings.lua" user-emacs-directory))))
    (add-to-list 'eglot-server-programs `(lua-mode . (,ig--sumneko-bin "-E" "-e" "LANG=en" ,ig--sumneko-main ,ig--sumneko-settings))))
  (add-to-list 'eglot-server-programs '(sh-mode . ("~/.cache/lspServers/bash/node_modules/.bin/bash-language-server" "start"))))

(use-package leuven-theme
  :straight t
  :config
  (load-theme 'leuven t))

(use-package savehist
  :defer 1
  :config
  (setq savehist-file "~/.cache/emacs/savehist")
  (savehist-mode))

(use-package recentf
  :defer 1
  :config
  (setq recentf-save-file "~/.cache/emacs/recentf"
	recentf-auto-cleanup 'never)
  (recentf-mode))

(global-display-line-numbers-mode)

(let* ((systemd-files '(".service" ".timer" ".target" ".mount" ".automount" ".slice" ".socket" ".path" ".netdev" ".network" ".link"))
       (systemd-regexp (concat (regexp-opt systemd-files t) "\\'")))
  (add-to-list 'auto-mode-alist (cons systemd-regexp 'conf-unix-mode)))
