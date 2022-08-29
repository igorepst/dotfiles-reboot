
  
  (defun shk-eshell-prompt ()
    (let ((header-bg "#fff"))
      (concat
       ;; (with-face "\n" :background header-bg)
       (with-face (concat (abbreviate-file-name (eshell/pwd)) " ") :background header-bg)
       (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#888")
       (with-face
        (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
        :background header-bg)
     
       ;; (with-face user-login-name :foreground "blue")
       ;; "@"
       ;; (with-face "localhost" :foreground "green")
       (with-face "\n" :background header-bg)
       (if (= (user-uid) 0)
           (with-face "#" :foreground "red")
         "$")
       " ")))
  (setq eshell-prompt-function 'shk-eshell-prompt)
  
(setq eshell-prompt-regexp "^[#$] ")


(setq eshell-highlight-prompt nil)


  (defun with-face (str &rest face-plist)
    (propertize str 'face face-plist))
(setq eshell-prompt-function
      (lambda ()
    (concat (abbreviate-file-name (eshell/pwd)) "\n"
            (with-face (if (= (user-uid) 0) "# " "$ ") :background "blue"))))
(setq eshell-prompt-regexp "^[#$] ")


(defun eshell-emit-prompt ()
  "Emit a prompt if eshell is being used interactively."
  (when (boundp 'ansi-color-context-region)
    (setq ansi-color-context-region nil))
  (run-hooks 'eshell-before-prompt-hook)
  (if (not eshell-prompt-function)
      (set-marker eshell-last-output-end (point))
    (let ((prompt (funcall eshell-prompt-function)))
      (and eshell-highlight-prompt
	   (add-text-properties 0 (length prompt)
				'(read-only t
				  font-lock-face eshell-prompt
				  front-sticky (font-lock-face read-only)
				  rear-nonsticky (font-lock-face read-only))
				prompt))
      (eshell-interactive-print prompt)))
  (run-hooks 'eshell-after-prompt-hook))
