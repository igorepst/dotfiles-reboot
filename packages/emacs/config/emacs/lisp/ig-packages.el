;;; ig-packages.el --- Packages configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of used packages.

;;; Code:

(require 'ig-common)
(defconst ig-selected-packages '() "Autogeneration of package-selected-packages.")

(push 'lua-mode ig-selected-packages)
(push '("\\.lua\\'" . lua-mode) auto-mode-alist)

;; "service" "timer" "target" "slice" "socket" "path" "network" "automount" "link" "mount" "netdev"
(push '("\\.\\(?:automount\\|link\\|mount\\|net\\(?:dev\\|work\\)\\|path\\|s\\(?:ervice\\|lice\\|ocket\\)\\|t\\(?:arget\\|imer\\)\\)\\'" . conf-unix-mode) auto-mode-alist)

(with-eval-after-load 'savehist
  (setq savehist-file (concat ig-cache-dir "savehist")
	history-length 250
	savehist-additional-variables
	'(search-ring regexp-search-ring compile-history kill-ring
		      shell-command-history vertico-repeat-history)))

(with-eval-after-load 'saveplace
  (setq save-place-file (concat ig-cache-dir "saveplace")
	save-place-version-control 'never
	save-place-ignore-files-regexp
	(concat "\\(?:" ig-kitty-scrollback-file "\\|COMMIT_EDITMSG\\|MERGE_MSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$")))
(save-place-mode)

(with-eval-after-load 'recentf
  (setq recentf-save-file (concat ig-cache-dir "recentf")
	recentf-auto-cleanup 3600
	recentf-exclude `("MERGE_MSG" "COMMIT_EDITMSG" "bookmarks" ,ig-kitty-scrollback-file)))

(with-eval-after-load 'bookmark
  (setq bookmark-default-file (concat ig-cache-dir "bookmarks")))

(with-eval-after-load 'org
  (setq org-fontify-whole-heading-line t
	org-startup-with-inline-images t
	org-default-notes-file "~/notes/tasks.org"
	org-support-shift-select t))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
	`(("e" "Emacs WIP"  entry (file ,(concat user-emacs-directory "wip.org")) "* TODO %?" :empty-lines 1)
	  ("t" "Task" entry (file+headline org-default-notes-file "Tasks") "* TODO %?\n  %u\n  %a" :empty-lines 1))))

(with-eval-after-load 'isearch
  (setq isearch-lazy-count t
	isearch-lazy-highlight t
	search-upper-case nil
	isearch-wrap-pause 'no-ding)
  ;; http://stackoverflow.com/a/287067/407953
  ;; TODO rewrite to use newer advice-add
  (defadvice isearch-search (after isearch-no-fail activate)
    "Wrap isearch automatically."
    (unless isearch-success
      (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
      (ad-activate 'isearch-search)
      (isearch-repeat (if isearch-forward 'forward))
      (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
      (ad-activate 'isearch-search)))
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char))

(let ((inhibit-message t))
  (global-hl-line-mode)
  (recentf-mode)
  (savehist-mode)
  (global-auto-revert-mode)
  (delete-selection-mode)
  (column-number-mode)
  (global-goto-address-mode)
  (repeat-mode))
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))



(push 'dired-rsync ig-selected-packages)

(with-eval-after-load 'dired
  (require 'ig-dired))



(with-eval-after-load 'man
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline nil))

(add-hook 'find-file-hook 'ig-find-file-root-header-warning)
(add-hook 'dired-mode-hook 'ig-find-file-root-header-warning)



(push 'vertico ig-selected-packages)
(vertico-mode)
(setq vertico-cycle t)

(define-key vertico-map [?\t] #'vertico-insert-unless-tramp)
(define-key vertico-map [\M-up] #'vertico-directory-delete-entry)
(define-key vertico-map [\M-right] #'vertico-directory-enter)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(defvar +vertico-transform-functions nil)

(defun +vertico-transform (args)
  "Transform Vertico ARGS."
  (dolist (fun (ensure-list +vertico-transform-functions) args)
    (setcar args (funcall fun (car args)))))

(advice-add #'vertico--format-candidate :filter-args #'+vertico-transform)

(defun +vertico-highlight-directory (file)
  "Highlight FILE if it ends with a slash."
  (if (string-suffix-p "/" file)
      (propertize file 'face 'marginalia-file-priv-dir)
    file))

(defun +vertico-sort-directories-first (files)
  "Sort FILES by directory-first."
  (setq files (vertico-sort-alpha files))
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

(setq vertico-multiform-commands
      '((find-file #'vertico-multiform-vertical
		   (vertico-sort-function . +vertico-sort-directories-first)
		   (+vertico-transform-functions . +vertico-highlight-directory))
	(dired #'vertico-multiform-vertical
               (vertico-sort-function . +vertico-sort-directories-first)
               (+vertico-transform-functions . +vertico-highlight-directory))
	(consult-grep buffer)
	(consult-ripgrep buffer)
	(consult-git-grep buffer)))

(vertico-multiform-mode)



(push 'orderless ig-selected-packages)
(defun basic-remote-try-completion (string table pred point)
  "Support TRAMP hostname completion with STRING, TABLE, PRED, POINT."
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))
(defun basic-remote-all-completions (string table pred point)
  "Support TRAMP hostname completion with STRING, TABLE, PRED, POINT."
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))
(push '(basic-remote basic-remote-try-completion basic-remote-all-completions nil) completion-styles-alist)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles basic-remote substring partial-completion))))



(push 'marginalia ig-selected-packages)
(marginalia-mode)



(push 'consult ig-selected-packages)
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format
      xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)
(advice-add #'register-preview :override #'consult-register-window)

(with-eval-after-load 'consult
  (setq consult--customize-alist
	`((,#'consult-buffer-other-frame :preview-key nil)
	  (,#'consult-ripgrep :preview-key [?\C-\;])
	  (,#'consult-git-grep :preview-key [?\C-\;])
	  (,#'consult-grep :preview-key [?\C-\;])
	  (,#'consult-bookmark :preview-key [?\C-\;])
	  (,#'consult-recent-file :preview-key [?\C-\;])
	  (,#'consult-xref :preview-key [?\C-\;])
	  (,#'consult--source-bookmark :preview-key [?\C-\;])
	  (,#'consult--source-recent-file :preview-key [?\C-\;])
	  (,#'consult--source-project-recent-file :preview-key [?\C-\;])
	  (,#'consult-theme :preview-key '(:debounce 0.2 any)))
	consult-narrow-key [?\C-+]
	consult-ripgrep-args
	(concat "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --ignore-case --no-heading --line-number --ignore-file " (getenv "HOME") "/.config/ripgrep/ignore --hidden ."))
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(define-key global-map "\C-ch" 'consult-history)
(define-key global-map "\C-x\M-:" 'consult-complex-command) ;; orig. repeat-complex-command
(define-key global-map "\C-xb" 'consult-buffer)
(define-key global-map "\C-x\C-b" 'consult-buffer)
(define-key global-map "\C-x4b" 'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
(define-key global-map "\C-x5b" 'consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
(define-key global-map "\C-xrb" 'consult-bookmark) ;; orig. bookmark-jump
(define-key global-map "\C-xpb" 'consult-project-buffer) ;; orig. project-switch-to-buffer
(define-key global-map "\M-y" 'consult-yank-from-kill-ring) ;; orig. yank-pop
(define-key 'help-command "a" 'consult-apropos) ;; orig. apropos-command
(define-key global-map "\M-ge" 'consult-compile-error)
(define-key global-map "\M-gg" 'consult-goto-line) ;; orig. goto-line
(define-key global-map "\M-gi" 'consult-imenu)
(define-key global-map "\M-go" 'consult-outline)
(define-key global-map "\M-sd" 'consult-find)
(define-key global-map "\M-sr" 'consult-ripgrep)
(define-key global-map "\M-sl" 'consult-line)
(define-key global-map "\M-sL" 'consult-line-multi)
(define-key global-map "\M-sk" 'ig-consult-keep-lines)
(define-key global-map "\M-su" 'consult-focus-lines)
(define-key global-map "\M-se" 'consult-isearch-history)
(define-key isearch-mode-map "\M-e" 'consult-isearch-history) ;; orig. isearch-edit-string
(define-key isearch-mode-map "\M-se" 'consult-isearch-history) ;; orig. isearch-edit-string
(define-key isearch-mode-map "\M-sl" 'consult-line) ;; needed by consult-line to detect isearch
(define-key isearch-mode-map "\M-sL" 'consult-line-multi) ;; needed by consult-line to detect isearch
(define-key minibuffer-local-map "\M-s" 'consult-history) ;; orig. next-matching-history-element
(define-key minibuffer-local-map "\M-r" 'consult-history) ;; orig. previous-matching-history-element

(push 'consult-flycheck ig-selected-packages)
(define-key global-map "\M-gf" 'consult-flycheck)



(push 'corfu ig-selected-packages)
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

(push 'kind-icon ig-selected-packages)
(with-eval-after-load 'corfu
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(with-eval-after-load 'svg-lib
  (setq svg-lib-icons-dir (concat ig-cache-dir "svg-lib")))



(push 'cape ig-selected-packages)
(push #'cape-file completion-at-point-functions)
(push #'cape-dabbrev completion-at-point-functions)
(define-key global-map "\C-cpf" 'cape-file)

(add-hook 'emacs-lisp-mode-hook #'cape-elisp)
;; Sanitize the `pcomplete-completions-at-point' Capf.
;; The Capf has undesired side effects on Emacs 28 and earlier.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)



(push 'embark ig-selected-packages)
(setq prefix-help-command #'embark-prefix-help-command
      embark-prompter #'embark-completing-read-prompter
      embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
(with-eval-after-load 'embark
  (push '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" nil
	  (window-parameters
	   (mode-line-format . none))) display-buffer-alist)
  (require 'ig-embark-this-file))
(define-key global-map [?\C-.] 'embark-act)
(define-key global-map "\M-." 'embark-dwim)
(define-key global-map "\C-hB" 'embark-bindings)
(define-key global-map "\C-co" #'ig-embark-act-on-buffer-file)

(push 'embark-consult ig-selected-packages)
(with-eval-after-load 'consult
  (with-eval-after-load 'embark
    (require 'embark-consult)
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))



(push 'flycheck ig-selected-packages)
(add-hook 'prog-mode-hook 'flycheck-mode)
(with-eval-after-load 'flycheck
  (setq flycheck-emacs-lisp-load-path 'inherit))

(push 'package-lint ig-selected-packages)



(push 'lsp-mode ig-selected-packages)
(push 'lsp-ui ig-selected-packages)
(push 'consult-lsp ig-selected-packages)
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-log-io nil
	lsp-enable-suggest-server-download nil
	lsp-session-file (concat ig-cache-dir "lsp-session-v1")
	lsp-warn-no-matched-clients nil
	lsp-enable-snippet nil
	lsp-completion-provider :none
	lsp-lua-diagnostics-globals ["vim" "awesome" "client" "screen" "tag" "mouse" "keygrabber"])
  (let* ((ig--sumneko-root-path (expand-file-name "~/.cache/lspServers/lua/sumneko-lua/extension/server/"))
	 (ig--sumneko-bin (concat ig--sumneko-root-path "bin/lua-language-server"))
	 (ig--sumneko-main (concat ig--sumneko-root-path "main.lua")))
    (setq lsp-clients-lua-language-server-install-dir ig--sumneko-root-path
	  lsp-clients-lua-language-server-bin ig--sumneko-bin
	  lsp-clients-lua-language-server-main-location ig--sumneko-main)))
(add-hook 'lua-mode-hook 'lsp-deferred)
(add-hook 'sh-mode-hook 'lsp-deferred)

(push 'lsp-pyright ig-selected-packages)
(add-hook 'python-mode-hook (lambda () (require 'lsp-pyright) (lsp-deferred)))



(push 'rainbow-delimiters ig-selected-packages)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)



(with-eval-after-load 'ansi-color
  (set-face-attribute 'ansi-color-black nil :foreground ig-color-black :background ig-color-black)
  (set-face-attribute 'ansi-color-red nil :foreground ig-color-red :background ig-color-red)
  (set-face-attribute 'ansi-color-green nil :foreground ig-color-green :background ig-color-green)
  (set-face-attribute 'ansi-color-yellow nil :foreground ig-color-yellow :background ig-color-yellow)
  (set-face-attribute 'ansi-color-blue nil :foreground ig-color-blue :background ig-color-blue)
  (set-face-attribute 'ansi-color-magenta nil :foreground ig-color-magenta :background ig-color-magenta)
  (set-face-attribute 'ansi-color-cyan nil :foreground ig-color-cyan :background ig-color-cyan)
  (set-face-attribute 'ansi-color-white nil :foreground ig-color-white :background ig-color-white)
  (set-face-attribute 'ansi-color-bright-black nil :foreground ig-color-bright-black :background ig-color-bright-black)
  (set-face-attribute 'ansi-color-bright-red nil :foreground ig-color-bright-red :background ig-color-bright-red)
  (set-face-attribute 'ansi-color-bright-green nil :foreground ig-color-bright-green :background ig-color-bright-green)
  (set-face-attribute 'ansi-color-bright-yellow nil :foreground ig-color-bright-yellow :background ig-color-bright-yellow)
  (set-face-attribute 'ansi-color-bright-blue nil :foreground ig-color-bright-blue :background ig-color-bright-blue)
  (set-face-attribute 'ansi-color-bright-magenta nil :foreground ig-color-bright-magenta :background ig-color-bright-magenta)
  (set-face-attribute 'ansi-color-bright-cyan nil :foreground ig-color-bright-cyan :background ig-color-bright-cyan)
  (set-face-attribute 'ansi-color-bright-white nil :foreground ig-color-bright-white :background ig-color-bright-white))



(push 'xterm-color ig-selected-packages)
(with-eval-after-load 'xterm-color
  (setq xterm-color-use-bold-for-bright t
	xterm-color-names `[,ig-color-black ,ig-color-red ,ig-color-green ,ig-color-yellow ,ig-color-blue ,ig-color-magenta ,ig-color-cyan ,ig-color-white]
	xterm-color-names-bright `[,ig-color-bright-black ,ig-color-bright-red ,ig-color-bright-green ,ig-color-bright-yellow
							  ,ig-color-bright-blue ,ig-color-bright-magenta ,ig-color-bright-cyan ,ig-color-bright-white]))

(with-eval-after-load 'eshell (require 'ig-eshell))
(add-hook 'eshell-mode-hook (lambda ()
			      ;; Jump to prompts with consult-outline
			      (setq outline-regexp eshell-prompt-regexp)
			      (add-hook 'eshell-pre-command-hook #'ig-eshell-pre-command nil t)
			      (add-hook 'eshell-post-command-hook #'ig-eshell-post-command nil t)
			      (setq-local global-hl-line-mode nil
					  imenu-generic-expression `(("Prompt" ,(concat eshell-prompt-regexp "\\(.*\\)") 1)))))
(with-eval-after-load 'gitstatusd
  (customize-set-variable 'gitstatusd-exe "~/.cache/gitstatus/gitstatusd-linux-x86_64"))
(with-eval-after-load 'gitstatus
  (set-face-attribute 'gitstatus-default-face nil :foreground ig-color-bright-black)
  (set-face-attribute 'gitstatus-clean-face nil :foreground ig-color-bright-green)
  (set-face-attribute 'gitstatus-modified-face nil :foreground ig-color-yellow)
  (set-face-attribute 'gitstatus-untracked-face nil :foreground ig-color-bright-blue)
  (set-face-attribute 'gitstatus-conflicted-face nil :foreground ig-color-bright-red))
(with-eval-after-load 'gitstatus-eshell
  (customize-set-variable 'gitstatus-eshell-neighbour-regex "\\( \\)")
  (customize-set-variable 'gitstatus-eshell-prompt-lines 2))
(add-hook 'eshell-before-prompt-hook #'gitstatus-eshell-start)



(with-eval-after-load 'ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
	ediff-split-window-function 'split-window-horizontally
	ediff-diff-options "-w"))



;; Do not ask to save the `sudo' password
(connection-local-set-profile-variables
 'remote-without-auth-sources '((auth-sources . nil)))
(connection-local-set-profiles
 '(:application tramp :protocol "sudo" :user "root") 'remote-without-auth-sources)
;; TODO do it for local `sudo' connections only???
(with-eval-after-load 'tramp
  (push (concat "TIME_STYLE=" (getenv "TIME_STYLE")) tramp-remote-process-environment))



(with-eval-after-load 're-builder
  (setq reb-re-syntax 'string))

(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(push 'modus-themes ig-selected-packages)
(load-theme 'modus-operandi t)
(set-cursor-color ig-color-orange)

(add-hook 'prog-mode-hook #'font-lock-comment-annotations)

(push '("\\.[Ll][Oo][Gg]\\'" . ig-font-lock-log-file) auto-mode-alist)

(makunbound 'ig-selected-packages)



(define-key 'help-command [? ] #'ig-describe-symbol)
(define-key 'help-command "\C-l" #'find-library)
(define-key 'help-command "\C-f" #'find-function)
(define-key 'help-command "\C-k" #'find-function-on-key)
(define-key 'help-command "\C-v" #'find-variable)
(define-key 'help-command "\C-c" #'describe-char)
(define-key global-map [C-tab] #'bury-buffer)
(define-key global-map [remap kill-line] #'ig-kill-current-line-or-region)
(define-key global-map "\M-g\M-g" 'ig-read-pathmarks-dwim)
(define-key global-map [C-return] #'ig-open-new-line)

(defvar ig-custom-map
  (let ((map (make-sparse-keymap)))
    (define-key map "'" #'ig-eshell-switch-or-new)
    map)
  "Custom map.")
(fset 'ig-custom-map ig-custom-map)
(define-key global-map [?\C-\,] #'ig-custom-map)

(defvar ig-custom-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'ig-duplicate-current-line-or-region)
    (define-key map "k" #'ig-kill-current-line-or-region)
    (define-key map "c" #'ig-copy-current-line-or-region)
    (define-key map "s" #'ig-select-current-line-or-region)
    map)
  "Custom line map.")
(fset 'ig-custom-line-map ig-custom-line-map)
(define-key ig-custom-map "l" #'ig-custom-line-map)

(defvar ig-custom-line-repeatable-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" #'ig-duplicate-current-line-or-region)
    (define-key map "k" #'ig-kill-current-line-or-region)
    map)
  "Custom repeatable line map.")
(put 'ig-duplicate-current-line-or-region 'repeat-map 'ig-custom-line-repeatable-map)
(put 'ig-kill-current-line-or-region 'repeat-map 'ig-custom-line-repeatable-map)

(defvar ig-custom-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" #'ig-indent-buffer)
    (define-key map "s" #'ig-sort-lines)
    (define-key map "r" #'ig-reverse-region)
    (define-key map "u" #'ig-find-alternative-file-with-sudo)
    map)
  "Custom buffer map.")
(fset 'ig-custom-buffer-map ig-custom-buffer-map)
(define-key ig-custom-map "b" #'ig-custom-buffer-map)

(defvar ig-custom-window-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" #'transpose-windows)
    map)
  "Custom window map.")
(fset 'ig-custom-window-map ig-custom-window-map)
(put 'transpose-windows 'repeat-map 'ig-custom-window-map)
(define-key ig-custom-map "w" #'ig-custom-window-map)

(defvar ig-custom-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" #'to-and-from-minibuffer)
    (define-key map [down] #'minibuffer-down-from-outside)
    (define-key map [up] #'minibuffer-up-from-outside)
    map)
  "Custom minibuffer map.")
(fset 'ig-custom-minibuffer-map ig-custom-minibuffer-map)
(define-key ig-custom-map "m" #'ig-custom-minibuffer-map)
(put 'to-and-from-minibuffer 'repeat-map 'ig-custom-minibuffer-map)
(put 'minibuffer-down-from-outside 'repeat-map 'ig-custom-minibuffer-map)
(put 'minibuffer-up-from-outside 'repeat-map 'ig-custom-minibuffer-map)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-packages)
;;; ig-packages.el ends here
