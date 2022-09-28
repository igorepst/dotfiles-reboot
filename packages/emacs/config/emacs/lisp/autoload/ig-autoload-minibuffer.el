;;; ig-autoload-minibuffer.el --- Autoload minbuffer configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of autoload for minibuffer.

;;; Code:

;; https://github.com/minad/vertico/wiki#useful-commands-from-outside-minibuffer
;;;###autoload
(defun minibuffer-down-from-outside ()
  "Move to next candidate in minibuffer, even when minibuffer isn't selected."
  (interactive)
  (with-selected-window (active-minibuffer-window)
    (vertico-next)
    (vertico--exhibit)
    (when consult--preview-function
      (funcall consult--preview-function))))

;;;###autoload
(defun minibuffer-up-from-outside ()
  "Move to previous candidate in minibuffer, even when minibuffer isn't selected."
  (interactive)
  (with-selected-window (active-minibuffer-window)
    (vertico-previous)
    (vertico--exhibit)
    (when consult--preview-function
      (funcall consult--preview-function))))

;;;###autoload
(defun to-and-from-minibuffer ()
  "Go back and forth between minibuffer and other window."
  (interactive)
  (if (window-minibuffer-p (selected-window))
      (select-window (minibuffer-selected-window))
    (select-window (active-minibuffer-window))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-autoload-minibuffer)
;;; ig-autoload-minibuffer.el ends here
