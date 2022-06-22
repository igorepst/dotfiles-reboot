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

(with-eval-after-load 'recentf
(setq recentf-save-file "~/.cache/emacs/recentf"
      recentf-auto-cleanup 'never
      recentf-exclude '("MERGE_MSG" "COMMIT_EDITMSG")))

(with-eval-after-load 'org
  (setq org-fontify-whole-heading-line t
	org-startup-with-inline-images t))

(with-eval-after-load 'isearch
  (setq isearch-lazy-count t
	isearch-lazy-highlight t
	search-upper-case nil
	isearch-wrap-pause 'no-ding))

(run-with-idle-timer 0.1 nil (lambda()
			       (let ((inhibit-message t))
				 (recentf-mode))
			       (with-current-buffer "*scratch*"
				 (emacs-lock-mode 'kill))
			       (with-current-buffer "*Messages*"
				 (emacs-lock-mode 'kill))
			       (delete-selection-mode)
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



(straight-use-package 'orderless)
(setq completion-styles '(substring orderless basic)
      completion-category-overrides '((file (styles basic substring partial-completion))))

;; (customize-set-variable 'completion-styles '(substring orderless basic))



(straight-use-package 'marginalia)
(marginalia-mode)
(add-hook 'minibuffer-setup-hook (lambda() (define-key minibuffer-local-map "\M-A" 'marginalia-cycle)))



(straight-use-package 'consult)
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

;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)
(with-eval-after-load 'consult
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
(define-key global-map "\C-x\C-a" 'consult-recent-file)
(define-key global-map "\C-ch" 'consult-history)
(define-key global-map "\C-ck" 'consult-kmacro)
(define-key global-map "\C-x\M-:" 'consult-complex-command) ;; orig. repeat-complex-command
(define-key global-map "\C-xb" 'consult-buffer) ;; orig. switch-to-buffer
(define-key global-map "\C-x4b" 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(define-key global-map "\C-x5b" 'consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
(define-key global-map "\C-xrb" 'consult-bookmark) ;; orig. bookmark-jump
(define-key global-map "\C-xpb" 'consult-project-buffer) ;; orig. project-switch-to-buffer
(define-key global-map "\M-#" 'consult-register-load)
(define-key global-map "\M-'" 'consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
(define-key global-map [?\C-\M-#] 'consult-register)
(define-key global-map "\M-y" 'consult-yank-pop) ;; orig. yank-pop
(define-key global-map [(help) (a)] 'consult-apropos) ;; orig. apropos-command
(define-key global-map "\M-ge" 'consult-compile-error)
(define-key global-map "\M-gg" 'consult-goto-line) ;; orig. goto-line
(define-key global-map "\M-g\M-g" 'consult-goto-line) ;; orig. goto-line
(define-key global-map "\M-go" 'consult-outline)
(define-key global-map "\M-gm" 'consult-mark)
(define-key global-map "\M-gk" 'consult-global-mark)
(define-key global-map "\M-gi" 'consult-imenu)
(define-key global-map "\M-gI" 'consult-imenu-multi)
(define-key global-map "\M-sd" 'consult-find)
(define-key global-map "\M-sD" 'consult-locate)
(define-key global-map "\M-sg" 'consult-grep)
(define-key global-map "\M-sG" 'consult-git-grep)
(define-key global-map "\M-sr" 'consult-ripgrep)
(define-key global-map "\M-sl" 'consult-line)
(define-key global-map "\M-sL" 'consult-line-multi)
(define-key global-map "\M-sm" 'consult-multi-occur)
(define-key global-map "\M-sk" 'consult-keep-lines)
(define-key global-map "\M-su" 'consult-focus-lines)
(define-key global-map "\M-se" 'consult-isearch-history)
(define-key isearch-mode-map "\M-e" 'consult-isearch-history) ;; orig. isearch-edit-string
(define-key isearch-mode-map "\M-se" 'consult-isearch-history) ;; orig. isearch-edit-string
(define-key isearch-mode-map "\M-sl" 'consult-line) ;; needed by consult-line to detect isearch
(define-key isearch-mode-map "\M-sL" 'consult-line-multi) ;; needed by consult-line to detect isearch
(define-key minibuffer-local-map "\M-s" 'consult-history) ;; orig. next-matching-history-element
(define-key minibuffer-local-map "\M-r" 'consult-history) ;; orig. previous-matching-history-element

(straight-use-package 'consult-flycheck)
(define-key global-map "\M-gf" 'consult-flycheck)



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
(define-key global-map "\C-cpp" 'completion-at-point)
(define-key global-map "\C-cpt" 'complete-tag)
(define-key global-map "\C-cpd" 'cape-dabbrev)
(define-key global-map "\C-cph" 'cape-history)
(define-key global-map "\C-cpf" 'cape-file)
(define-key global-map "\C-cpk" 'cape-keyword)
(define-key global-map "\C-cps" 'cape-symbol)
(define-key global-map "\C-cpa" 'cape-abbrev)
(define-key global-map "\C-cpi" 'cape-ispell)
(define-key global-map "\C-cpl" 'cape-line)
(define-key global-map "\C-cpw" 'cape-dict)
(define-key global-map "\C-cp\\" 'cape-tex)
(define-key global-map "\C-cp&" 'cape-sgml)
(define-key global-map "\C-cpr" 'cape-rfc1345)



(straight-use-package 'embark)
(setq prefix-help-command #'embark-prefix-help-command)
(with-eval-after-load 'embark
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" nil
		 (window-parameters
		  (mode-line-format . none)))))
(define-key global-map [?\C-.] 'embark-act)
(define-key global-map "\M-." 'embark-dwim)
(define-key global-map "\C-hB" 'embark-bindings)

(straight-use-package 'embark-consult)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))



(straight-use-package 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
(with-eval-after-load 'flycheck
  (setq flycheck-emacs-lisp-load-path 'inherit))



(straight-use-package 'lsp-mode)
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
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
	  lsp-clients-lua-language-server-main-location ig--sumneko-main)))
(add-hook 'lua-mode-hook 'lsp-deferred)
(add-hook 'sh-mode-hook 'lsp-deferred)

(straight-use-package 'lsp-ui)

(straight-use-package 'lsp-pyright)
(add-hook 'python-mode-hook (lambda () (require 'lsp-pyright) (lsp-deferred)))

(straight-use-package 'consult-lsp)



(straight-use-package 'esup)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-packages)
;;; ig-packages.el ends here
