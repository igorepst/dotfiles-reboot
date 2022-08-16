;;; ig-embark-this-file.el --- Embark on the current file configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration to use Embark on the current file

;;; Code:

(require 'embark)

(defun embark-target-this-buffer-file ()
  "Target is the current file or buffer."
  (cons 'this-buffer-file (buffer-name)))

;; TODO change actions
(embark-define-keymap this-buffer-file-map
  "Commands to act on current file or buffer."
  ("l" load-file)
  ("b" byte-compile-file)
  ("S" ig-find-alternative-file-with-sudo)
  ("r" rename-file-and-buffer)
  ("d" diff-buffer-with-file)
  ("=" ediff-buffers)
  ("C-=" ediff-files)
  ("!" shell-command)
  ("&" async-shell-command)
  ("x" consult-file-externally)
  ("c" copy-file)
  ("k" kill-buffer)
  ("z" bury-buffer)
  ("|" embark-shell-command-on-buffer)
  ("g" revert-buffer)
  ("<" previous-buffer)
  (">" next-buffer)
  ("t" transpose-windows))

(add-to-list 'embark-target-finders #'embark-target-this-buffer-file t)
(add-to-list 'embark-keymap-alist '(this-buffer-file . this-buffer-file-map))
(push 'embark--allow-edit (alist-get 'write-file embark-target-injection-hooks))

(add-to-list 'embark-repeat-actions #'previous-buffer)
(add-to-list 'embark-repeat-actions #'next-buffer)
(add-to-list 'embark-repeat-actions #'transpose-windows)

;;;###autoload
(defun ig-embark-act-on-buffer-file (&optional arg)
  "Act on the current file or buffer with ARG."
  (interactive "P")
  (let ((embark-target-finders '(embark-target-this-buffer-file))
	(embark-keymap-alist '((this-buffer-file . this-buffer-file-map))))
    (embark-act arg)))

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
(provide 'ig-embark-this-file)
;;; ig-embark-this-file.el ends here
