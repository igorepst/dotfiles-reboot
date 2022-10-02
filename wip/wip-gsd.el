;;; wip-gsd.el --- GSD code -*- lexical-binding: t; -*-

;;; Commentary:
;; GSD

;;; Code:

(require 'cl-lib)

;;      4. Commit hash that HEAD is pointing to. 40 hex digits.
;;      5. Local branch name or empty if not on a branch.
;;      6. Upstream branch name. Can be empty.
;;      7. The remote name, e.g. "upstream" or "origin".
;;      8. Remote URL. Can be empty.
;;      9. Repository state, A.K.A. action. Can be empty.
;;     10. The number of files in the index.
;;     11. The number of staged changes.
;;     12. The number of unstaged changes.
;;     13. The number of conflicted changes.
;;     14. The number of untracked files.
;;     15. Number of commits the current branch is ahead of upstream.
;;     16. Number of commits the current branch is behind upstream.
;;     17. The number of stashes.
;;     18. The last tag (in lexicographical order) that points to the same
;;         commit as HEAD.
;;     19. The number of unstaged deleted files.
;;     20. The number of staged new files.
;;     21. The number of staged deleted files.
;;     22. The push remote name, e.g. "upstream" or "origin".
;;     23. Push remote URL. Can be empty.
;;     24. Number of commits the current branch is ahead of push remote.
;;     25. Number of commits the current branch is behind push remote.
;;     26. Number of files in the index with skip-worktree bit set.
;;     27. Number of files in the index with assume-unchanged bit set.
;;     28. Encoding of the HEAD's commit message. Empty value means UTF-8.
;;     29. The first paragraph of the HEAD's commit message as one line.


(cl-defstruct (gsd-resp
	       (:constructor nil)
	       (:constructor gsd-resp-create (req-id is-git-repo &optional abs-path))
	       (:copier nil))
  "Gitstatusd response."
  (req-id nil :documentation "Request id. The same as the first field in the request.")
  (is-git-repo nil :documentation "1 - if git repo. 0 - otherwise, all the following is missing.")
  (abs-path nil :documentation "Absolute path to the git repository workdir."))

(defun gsd-filter (_ str)
  "Filter GSD STR output."
  (let* ((res (split-string str ""))
	 (ret (gsd-resp-create (nth 0 res) (nth 1 res) (nth 2 res))))
    (message (if (gsd-resp-p ret) "YES" "NO"))
    (message (format "%s" res)))
  )

;; ~/.cache/gitstatus/gitstatusd-linux-x86_64 -G v1.5.4 -s -1 -u -1 -d -1 -c -1 -m -1 -v FATAL -t 16
(let ((proc
       (make-process
	:name "gsd"
	:buffer "gsd"
	:connection-type 'pipe
	:filter #'gsd-filter
	:command `(,(expand-file-name "~/.cache/gitstatus/gitstatusd-linux-x86_64")
		   "-G" "v1.5.4" "-s" "-1" "-u" "-1" "-d" "-1" "-c" "-1" "-m" "-1" "-v" "FATAL" "-t" "16"
		   ))
       ))
  

  (process-send-string proc (concat "ddd" "" (expand-file-name "~/dotfiles-reboot") "0"))

  )


(provide 'gsd)
;;; wip-gsd.el ends here

