;;; wip-gsd.el --- GSD code -*- lexical-binding: t; -*-

;;; Commentary:
;; GSD

;;; Code:

;; ~/.cache/gitstatus/gitstatusd-linux-x86_64 -G v1.5.4 -s -1 -u -1 -d -1 -c -1 -m -1 -v FATAL -t 16
(let ((proc
(make-process
 :name "gsd"
 :buffer "gsd"
 :connection-type 'pipe
 :command `(,(expand-file-name "~/.cache/gitstatus/gitstatusd-linux-x86_64") "-G" "v1.5.4" "-s" "-1" "-u" "-1" "-d" "-1" "-c" "-1" "-m" "-1" "-v" "FATAL" "-t" "16"))
))
  

  (process-send-string proc (concat "ddd" "" (expand-file-name " ~/dotfiles-reboot") "0"))

  )


(provide 'gsd)
;;; wip-gsd.el ends here

