;;; ig-fonts.el --- Set right fonts -*- lexical-binding: t; -*-

;;; Commentary:
;; Set right fonts

;;; Code:

(defun ig-font-override()
  "Set fonts overrides."
  (run-with-timer 0.5 nil (lambda()
			    (dolist (charset '((#x1f000 . #x1fbf9)))
			      (set-fontset-font t charset "Noto Sans Symbols2-14" nil 'append)))))

(defun ig-daemon-font ()
  "Font overrides helper."
  (remove-hook 'server-after-make-frame-hook #'ig-daemon-font)
  (ig-font-override))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'ig-daemon-font)
  (ig-font-override))

(provide 'ig-fonts)
;;; ig-fonts.el ends here
