;;; ig-common.el --- Common definitions -*- lexical-binding: t; -*-

;;; Commentary:
;; Common definitions. Make Flycheck happy

;;; Code:

(defconst ig-cache-dir "~/.cache/emacs" "Emacs cache directory.")
(defconst ig-ls-switches "-alh --group-directories-first --time-style \"+%d-%m-%Y %H:%M\"" "'ls' switches.")

(provide 'ig-common)
;;; ig-common.el ends here
