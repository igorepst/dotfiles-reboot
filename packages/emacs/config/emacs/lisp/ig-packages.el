;;; ig-packages.el --- Packages configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of used packages.

;;; Code:

(setq load-prefer-newer t
      straight-check-for-modifications nil
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
(eval-when-compile (require 'use-package))

(straight-use-package 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

(add-to-list 'auto-mode-alist
	     '("\\(\\.\\(?:service\\|timer\\|target\\|slice\\|socket\\|path\\|network\\|automount\\|link\\|mount\\|netdev\\)\\)\\'" . conf-unix-mode))

(load-theme 'adwaita t)

(setq savehist-file "~/.cache/emacs/savehist"
      savehist-additional-variables
      '(search-ring regexp-search-ring compile-history))
(savehist-mode)

(setq save-place-file "~/.cache/emacs/saveplace"
      save-place-version-control 'never
      save-place-ignore-files-regexp
      "\\(?:COMMIT_EDITMSG\\|MERGE_MSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$")
(save-place-mode)

(setq recentf-save-file "~/.cache/emacs/recentf"
      recentf-auto-cleanup 'never
      recentf-exclude '("MERGE_MSG" "COMMIT_EDITMSG"))
(recentf-mode)

(with-eval-after-load 'org
  (setq org-fontify-whole-heading-line t
	org-startup-with-inline-images t))

(with-eval-after-load 'isearch
  (setq isearch-lazy-count t
	isearch-lazy-highlight t
	search-upper-case nil
	isearch-wrap-pause 'no-ding))

(run-with-idle-timer 1 nil (lambda()
			     (delete-selection-mode)
			     (with-current-buffer "*scratch*"
			       (emacs-lock-mode 'kill))
			     (with-current-buffer "*Messages*"
			       (emacs-lock-mode 'kill))
			     ))

(with-eval-after-load 'dired
  (setq dired-use-ls-dired t
	dired-listing-switches "-alh --group-directories-first --time-style \"+%d-%m-%Y %H:%M\""))

(with-eval-after-load 'man
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline nil))

(straight-use-package 'vertico)
(vertico-mode)

(add-to-list 'load-path "~/.cache/emacs/straight/build/vertico/extensions/")
(autoload #'vertico-directory-up "vertico-directory" nil t)
(define-key vertico-map [left] 'vertico-directory-up)

(use-package orderless
  :straight t
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-overrides '((file (styles basic substring partial-completion)))))

(straight-use-package 'marginalia)
(marginalia-mode)
(add-hook 'minibuffer-setup-hook (lambda()
				   (define-key minibuffer-local-map "\M-A" 'marginalia-cycle)))

(use-package consult
  :straight t
  :bind (;; C-c bindings (mode-specific-map)
	 ("C-x C-a" . consult-recent-file)
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

(straight-use-package 'corfu)
;; Optional customizations
;; :custom
;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;; (corfu-auto t)                 ;; Enable auto completion
;; (corfu-separator ?\s)          ;; Orderless field separator
;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;; (corfu-preview-current nil)    ;; Disable current candidate preview
;; (corfu-preselect-first nil)    ;; Disable candidate preselection
;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;; (corfu-scroll-margin 5)        ;; Use scroll margin

;; Enable Corfu only for certain modes.
;; :hook ((prog-mode . corfu-mode)
;;        (shell-mode . corfu-mode)
;;        (eshell-mode . corfu-mode))

;; Recommended: Enable Corfu globally.
;; This is recommended since Dabbrev can be used globally (M-/).
;; See also `corfu-excluded-modes'.
(global-corfu-mode)
(defun corfu-enable-in-minibuffer ()
  "Enable Corfu in the minibuffer if `completion-at-point' is bound."
  (when (where-is-internal #'completion-at-point (list (current-local-map)))
    ;; (setq-local corfu-auto nil) Enable/disable auto completion
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)

(straight-use-package 'cape)
 (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
;;(add-to-list 'completion-at-point-functions #'cape-line)
(global-set-key (kbd "C-c p p") 'completion-at-point)
(global-set-key (kbd "C-c p t") 'complete-tag)
(global-set-key (kbd "C-c p d") 'cape-dabbrev)
(global-set-key (kbd "C-c p h") 'cape-history)
(global-set-key (kbd "C-c p f") 'cape-file)
(global-set-key (kbd "C-c p k") 'cape-keyword)
(global-set-key (kbd "C-c p s") 'cape-symbol)
(global-set-key (kbd "C-c p a") 'cape-abbrev)
(global-set-key (kbd "C-c p i") 'cape-ispell)
(global-set-key (kbd "C-c p l") 'cape-line)
(global-set-key (kbd "C-c p w") 'cape-dict)
(global-set-key (kbd "C-c p \\") 'cape-tex)
(global-set-key (kbd "C-c p &") 'cape-sgml)
(global-set-key (kbd "C-c p r") 'cape-rfc1345)

(straight-use-package 'embark)
(setq prefix-help-command #'embark-prefix-help-command)
(with-eval-after-load 'embark
  		 (add-to-list 'display-buffer-alist
			      '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" nil
				(window-parameters
				 (mode-line-format . none)))))
(global-set-key (kbd "C-.") 'embark-act)
(global-set-key (kbd "M-.") 'embark-dwim)
(global-set-key (kbd "C-h B") 'embark-bindings)

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(straight-use-package 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
(with-eval-after-load 'flycheck
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-log-io nil
	lsp-enable-suggest-server-download nil
	lsp-session-file "~/.cache/emacs/lsp-session-v1"
	lsp-warn-no-matched-clients nil
	lsp-enable-snippet nil
	lsp-completion-provider :none
	lsp-lua-diagnostics-globals ["vim" "awesome" "client" "screen" "tag" "mouse" "keygrabber"])
  (let* ((ig--sumneko-root-path "~/.cache/lspServers/lua/sumneko-lua/extension/server")
	 (ig--sumneko-bin (expand-file-name "bin/lua-language-server" ig--sumneko-root-path))
	 (ig--sumneko-main (expand-file-name "main.lua" ig--sumneko-root-path)))
    (setq lsp-clients-lua-language-server-install-dir ig--sumneko-root-path
	  lsp-clients-lua-language-server-bin ig--sumneko-bin
	  lsp-clients-lua-language-server-main-location ig--sumneko-main))
  :hook ((lua-mode . lsp-deferred)
         (sh-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(straight-use-package 'lsp-ui)

(straight-use-package 'lsp-pyright)
(add-hook 'python-mode-hook
	  #'(lambda nil
	      (require 'lsp-pyright)
	      (lsp-deferred)))

(straight-use-package 'consult-lsp)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-packages)
;;; ig-packages.el ends here
