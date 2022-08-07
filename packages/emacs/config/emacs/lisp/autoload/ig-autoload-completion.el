;;; ig-autoload-completion.el --- Autoload completion configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of autoload completion.

;;; Code:

;;;###autoload
(defun vertico-directory-delete-entry ()
  "Delete directory or entire entry before point."
  (interactive)
  (when (and (> (point) (minibuffer-prompt-end))
             (eq 'file (vertico--metadata-get 'category)))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (delete-region (1+ (point)) (point-max))
        t))))

;;https://github.com/minad/vertico/issues/240
;;;###autoload
 (defun vertico-insert-unless-tramp ()
   "Insert current candidate in minibuffer, except for TRAMP."
   (interactive)
   (if (vertico--remote-p (vertico--candidate))
	(minibuffer-complete)
     (vertico-insert)))

;;;###autoload
(defun cape-elisp ()
  "Define Cape for Elisp."
  (setq-local completion-at-point-functions
              (list (cape-super-capf #'elisp-completion-at-point #'cape-dabbrev) #'cape-file)))

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
(provide 'ig-autoload-completion)
;;; ig-autoload-completion.el ends here
