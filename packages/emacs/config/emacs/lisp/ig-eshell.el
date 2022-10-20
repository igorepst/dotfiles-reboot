;;; ig-eshell.el --- Eshell configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of eshell.

;;; Code:

(require 'eshell)
(require 'esh-mode)
(require 'esh-cmd)
(require 'em-hist)
(require 'em-dirs)
(require 'em-prompt)
(require 'vc-git)
(require 'ig-common)
(require 'em-tramp)
(require 'esh-module)

(with-eval-after-load 'esh-mode
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

(defvar-local ig-eshell-last-command-status 0)
(defvar-local ig-eshell-last-command-start-time nil)
(defvar-local ig-eshell-last-command-time nil)

(defconst ig-eshell-dir (concat ig-cache-dir "eshell/") "Eshell volatile dir.")
(make-directory ig-eshell-dir t)

(delete 'eshell-banner eshell-modules-list)
(push 'eshell-tramp eshell-modules-list)

(setq eshell-history-file-name (concat ig-eshell-dir "history")
      eshell-history-size 1000
      eshell-hist-ignoredups 'erase
      eshell-last-dir-ring-file-name (concat ig-eshell-dir "lastdir")
      eshell-buffer-maximum-lines 10240
      eshell-highlight-prompt nil
      eshell-prompt-regexp "^[#❯] "
      eshell-prompt-function #'ig-eshell-prompt
      xterm-color-preserve-properties t
      ;; Enable password caching by using sudo from em-tramp
      eshell-prefer-lisp-functions t
      eshell-prefer-lisp-variables t
      password-cache t
      password-cache-expiry 3600)
(setenv "TERM" "xterm-256color")
;; Autoloaded commands
(push "gpl" eshell-complex-commands)
(push "gps" eshell-complex-commands)
(push "q" eshell-complex-commands)

(defun ig-with-face (str &rest fpl)
  "Propertize STR with FPL."
  (propertize str 'face fpl))

(defun ig-eshell-pre-command()
  "Run on Eshell pre command."
  (setq ig-eshell-last-command-start-time (current-time)))

(defun ig-eshell-post-command()
  "Run on Eshell post command."
  (if ig-eshell-last-command-start-time
      (setq ig-eshell-last-command-time
	    (let ((last-cmd-time (float-time (time-subtract (current-time) ig-eshell-last-command-start-time))))
	      (if (> 1 last-cmd-time) nil (format "%.0fs" last-cmd-time)))
	    ig-eshell-last-command-start-time nil)
    (setq ig-eshell-last-command-time nil))
  (setq ig-eshell-last-command-status eshell-last-command-status)
  (eshell-interactive-print "\n"))

(defun ig-eshell-prompt()
  "Define custom Eshell prompt."
  (let* ((cur-dir (abbreviate-file-name (eshell/pwd)))
	 (prompt (concat
		  (ig-with-face (concat (if (string-equal cur-dir "~") " " " ") cur-dir) :weight 'bold :foreground ig-color-blue)
		  (ig-with-face (concat "  " (format-time-string "%H:%M:%S" (current-time))
					(when ig-eshell-last-command-time (concat "  " ig-eshell-last-command-time)))
				:foreground ig-color-bright-blue)
		  "\n"
		  (ig-with-face (if (zerop (user-uid)) "#" "❯")
				:foreground (if (zerop ig-eshell-last-command-status) ig-color-green ig-color-red))
		  " ")))
    (add-text-properties 0 (length prompt) '(read-only t
						       front-sticky (read-only)
						       rear-nonsticky (read-only))
			 prompt)
    prompt))

(define-key eshell-mode-map [\M-up] #'ig-eshell-up)
(define-key eshell-mode-map "\C-l" #'ig-eshell-clear)
;; Various issues prevent mapping to eshell/q directly
(define-key eshell-mode-map "\C-d" #'ig-eshell-kill-window)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-eshell)
;;; ig-eshell.el ends here
