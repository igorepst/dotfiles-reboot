;;; ig-packages.el --- Packages configuration

;;; Commentary:
;; Configuration of used packages.

;;; Code:

(setq straight-check-for-modifications nil
      straight-repository-branch "develop"
      straight-base-dir "~/.cache/emacs")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq use-package-enable-imenu-support t)
(straight-use-package 'use-package)

(use-package lua-mode
  :defer t
  :straight t
  :mode "\\.lua\\'")

(use-package conf-mode
  :defer t
  :straight (:type built-in)
  :mode ("\\(\\.\\(?:service\\|timer\\|target\\|slice\\|socket\\|path\\|network\\|automount\\|link\\|mount\\|netdev\\)\\)\\'" . conf-unix-mode))

(use-package project
  :defer t
  :straight (:type built-in))

(use-package xref
  :defer t
  :straight (:type built-in))

(use-package flymake
  :defer t
  :straight (:type built-in))

(use-package eldoc
  :defer t
  :straight (:type built-in))

(use-package jsonrpc
  :defer t
  :straight (:type built-in))

(use-package eglot
  :straight t
  :commands (eglot eglot-ensure)
  :hook ((lua-mode sh-mode python-mode) . eglot-ensure)
  :config
  (let* ((ig--sumneko-root-path "~/.cache/lspServers/lua/sumneko-lua/extension/server")
	 (ig--sumneko-bin (expand-file-name "bin/lua-language-server" ig--sumneko-root-path))
	 (ig--sumneko-main (expand-file-name "main.lua" ig--sumneko-root-path))
	 (ig--sumneko-settings (concat "--configpath=" (expand-file-name "external/sumnekoSettings.lua" user-emacs-directory))))
    (add-to-list 'eglot-server-programs `(lua-mode . (,ig--sumneko-bin "-E" "-e" "LANG=en" ,ig--sumneko-main ,ig--sumneko-settings))))
  (add-to-list 'eglot-server-programs '(sh-mode . ("~/.cache/lspServers/bash/node_modules/.bin/bash-language-server" "start")))
  (add-to-list 'eglot-server-programs '(python-mode . ("~/.local/bin/pyright-langserver" "--stdio"))))

(use-package leuven-theme
  :straight t
  :config
  (load-theme 'leuven t))

(use-package savehist
  :straight (:type built-in)
  :config
  (setq savehist-file "~/.cache/emacs/savehist"
	savehist-additional-variables    
        '(search-ring regexp-search-ring compile-history))
  (savehist-mode))

(use-package saveplace
  :straight (:type built-in)
  :config
  (setq save-place-file "~/.cache/emacs/saveplace"
	save-place-version-control 'never
	save-place-ignore-files-regexp
	"\\(?:COMMIT_EDITMSG\\|MERGE_MSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$")
  (save-place-mode))

(use-package recentf
  :straight (:type built-in)
  :config
  (setq recentf-save-file "~/.cache/emacs/recentf"
	recentf-auto-cleanup 'never
	recentf-exclude '("MERGE_MSG" "COMMIT_EDITMSG"))
  (recentf-mode))

(use-package isearch
  :defer t
  :straight (:type built-in)
  :config
  (setq isearch-lazy-count t
	isearch-lazy-highlight t
	search-upper-case nil
	isearch-wrap-pause 'no-ding))

(use-package delsel
  :straight (:type built-in)
  :config
  (delete-selection-mode))

(use-package emacs-lock
  :straight (:type built-in)
  :init
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))
  (with-current-buffer "*Messages*"
    (emacs-lock-mode 'kill)))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package vertico-directory
  :load-path "~/.cache/emacs/straight/build/vertico/extensions/"
  :bind
  (:map vertico-map
        ([left] . vertico-directory-up)))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package org-modern
  :straight t
  :hook ((org-mode org-agenda-finalize) . 'org-modern-mode))

(provide 'ig-packages)

;;; ig-packages.el ends here
