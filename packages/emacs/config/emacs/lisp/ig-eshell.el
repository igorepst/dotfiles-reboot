;;; ig-eshell.el --- Eshell configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of eshell.

;;; Code:

(require 'eshell)
(require 'vc-git)

(defvar-local ig-eshell-last-command-status 0)
(defvar-local ig-eshell-last-command-start-time nil)
(defvar-local ig-eshell-last-command-time nil)

(defconst ig-eshell-dir (expand-file-name "eshell" ig-cache-dir) "Eshell volatile dir.")
(make-directory ig-eshell-dir t)

(setq eshell-history-file-name (expand-file-name "history" ig-eshell-dir)
      eshell-last-dir-ring-file-name (expand-file-name "lastdir" ig-eshell-dir)
      eshell-hist-ignoredups 'erase
      eshell-banner-message ""
      eshell-buffer-maximum-lines 10240
      xterm-color-preserve-properties t
      eshell-highlight-prompt nil
      eshell-prompt-regexp "^[#❯] "
      eshell-prompt-function #'ig-eshell-prompt)
(setenv "TERM" "xterm-256color")

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

(defun ig-git-get-branch()
  "Return current Git branch."
  (with-temp-buffer
    (and
     (vc-git--out-ok "rev-parse" "--abbrev-ref" "HEAD")
     (buffer-substring-no-properties (point-min) ( - (point-max) 1)))))

(defun eshell/gps()
  "Git push."
  (vc-git--out-ok "push" "origin" (ig-git-get-branch)))

(defun ig-eshell-prompt()
  "Define custom Eshell prompt."
  (let* ((vc-be (vc-responsible-backend default-directory t))
	 (prompt (concat
		  (ig-with-face (abbreviate-file-name (eshell/pwd)) :weight 'bold :foreground ig-color-blue)
		  (when vc-be (ig-with-face (concat " on "
						    (if (eq 'Git vc-be)
							(concat " " (ig-git-get-branch))
						      (format "%s" vc-be)))
					    :foreground ig-color-magenta))
		  (ig-with-face (concat (format-time-string "  %H:%M" (current-time))
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

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-eshell)
;;; ig-eshell.el ends here
