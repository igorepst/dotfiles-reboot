;;; ig-autoload-eshell.el --- Autoload Eshell configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of autoload for Eshell.

;;; Code:

;;;###autoload
(defun ig-eshell-switch-or-new (&optional arg)
  "Create or switch to Eshell buffer with ARG."
  (interactive "P")
  (if (eq major-mode 'eshell-mode)
      (eshell (or arg t)) (eshell arg)))

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
  "Git push."
  (ig-eshell-git-cmd "pull"))

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
(provide 'ig-autoload-eshell)
;;; ig-autoload-eshell.el ends here