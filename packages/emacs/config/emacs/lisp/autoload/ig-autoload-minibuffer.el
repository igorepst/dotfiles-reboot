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
    (execute-kbd-macro [down])))

;;;###autoload
(defun minibuffer-up-from-outside ()
  "Move to previous candidate in minibuffer, even when minibuffer isn't selected."
  (interactive)
  (with-selected-window (active-minibuffer-window)
    (execute-kbd-macro [up])))

;;;###autoload
(defun to-and-from-minibuffer ()
  "Go back and forth between minibuffer and other window."
  (interactive)
  (if (window-minibuffer-p (selected-window))
      (select-window (minibuffer-selected-window))
    (select-window (active-minibuffer-window))))

(provide 'ig-autoload-minibuffer)
;;; ig-autoload-minibuffer.el ends here
