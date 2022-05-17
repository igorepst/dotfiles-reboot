;;; ig-packages.el --- Packages configuration

;;; Commentary:
;; Configuration of used packages.

;;; Code:

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
  :mode "\\.lua\\'")

(use-package conf-mode
  :defer t
  :mode ("\\(\\.\\(?:service\\|timer\\|target\\|slice\\|socket\\|path\\|network\\|automount\\|link\\|mount\\|netdev\\)\\)\\'" . conf-unix-mode))

(use-package eglot
  :straight t
  :commands (eglot eglot-ensure)
  :hook ((lua-mode sh-mode) . eglot-ensure)
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

(provide 'ig-packages)

;;; ig-packages.el ends here
