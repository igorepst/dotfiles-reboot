(push (list "\\(\\.mp4\\)$" '(".+" (dired-move-to-filename) nil (0 font-lock-variable-name-face))) dired-font-lock-keywords)




(defvar *original-dired-font-lock-keywords* dired-font-lock-keywords)
(defun dired-highlight-by-extensions (highlight-list)
  "highlight-list accept list of (regexp [regexp] ... face)."
  (let ((lst nil))
    (dolist (highlight highlight-list)
      (push `(, (concat "\\.\\(" (regexp-opt (butlast highlight)) "\\)$")
              (".+" (dired-move-to-filename)
               nil (0, (car (last highlight)))))
            lst))
    (setq dired-font-lock-keywords
          (append *original-dired-font-lock-keywords* lst))))


(dired-highlight-by-extensions
  '(("txt" font-lock-variable-name-face)
    ("lisp" "el" "pl" "c" "h" "cc" font-lock-variable-name-face)))
    
    
