;;; ig-autoload-eshell.el --- Autoload Eshell configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of autoload for Eshell.

;;; Code:

;;;###autoload
(defun ig-eshell-switch-or-new (&optional arg)
  "Create or switch to Eshell buffer with ARG."
  (interactive "P")
  (unless (window-in-direction 'below)
    (split-window-below))
  (windmove-down)
  (if (eq major-mode 'eshell-mode)
      (eshell (or arg t)) (eshell arg)))

;;;###autoload
(defun ig-eshell-up()
  "Go to the parent directory."
  (interactive)
  (let* ((cur-dir (expand-file-name default-directory))
	 (parent-dir (file-name-directory (directory-file-name cur-dir))))
    (unless (equal cur-dir parent-dir)
      (eshell-interactive-print (concat "cd " parent-dir))
      (eshell/cd parent-dir)
      (eshell-send-input))))

;;;###autoload
(defun eshell/q()
  "Exit Eshell."
  (interactive)
  (when (< 1 (length (window-list)))
    (delete-window))
  (eshell/exit))

(defun ig-git-get-branch()
  "Return current Git branch."
  (with-temp-buffer
    (and
     (vc-git--out-ok "rev-parse" "--abbrev-ref" "HEAD")
     (buffer-substring-no-properties (point-min) ( - (point-max) 1)))))

(defun ig-eshell-git-cmd (cmd)
  "Git helper for CMD."
  (let ((gb (ig-git-get-branch)))
    (when gb
      (throw 'eshell-replace-command
	     (eshell-parse-command (concat "*git " cmd " origin " gb))))))

;;;###autoload
(defun eshell/gps()
  "Git push."
  (ig-eshell-git-cmd "push"))

;;;###autoload
(defun eshell/gpl()
  "Git pull."
  (ig-eshell-git-cmd "pull"))

;;;###autoload
(defun ig-eshell-clear (&optional skip-prompt)
  "Clear screen, taking into account SKIP-PROMPT."
  (interactive)
  (eshell/clear-scrollback)
  (unless skip-prompt
    (eshell-emit-prompt)))

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
(provide 'ig-autoload-eshell)
;;; ig-autoload-eshell.el ends here
