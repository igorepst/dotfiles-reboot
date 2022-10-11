;;; gitstatusd.el --- `gitstatusd' wrapper -*- lexical-binding: t; -*-

;;; Commentary:
;; `gitstatusd' wrapper

;;; Code:

(require 'cl-lib)

(defgroup gitstatusd nil
  "Emacs interface to `gitstatusd'."
  :group 'tools)

(defcustom gitstatusd-exe "gitstatusd"
  "`Gitstatusd' executable path."
  :type '(string)
  :group 'gitstatusd)

(defcustom gitstatusd-cmd-args (concat "-s -1 -u -1 -d -1 -c -1 -m -1 -v FATAL -t " (format "%s" (* 2 (num-processors))))
  "`Gitstatusd' command line arguments."
  :type '(string)
  :group 'gitstatusd)

(defcustom gitstatusd-callback nil
  "Callback used when an answer arrives."
  :type '(function)
  :group 'gitstatusd)
;; (make-variable-buffer-local 'gitstatusd-callback)

(defcustom gitstatusd-compute-by-index t
  "Whether to perform computations by reading `git' index."
  :type 'boolean
  :group 'gitstatusd)

(defvar-local gitstatusd--proc nil "`Gitstatusd' process.")

(defconst gitstatusd--record-sep "" "Record separator.")

(defconst gitstatusd--unit-sep "" "Record separator.")

(cl-defstruct (gitstatusd
	       (:constructor nil)
	       (:constructor gitstatusd-create (req-id is-git-repo &optional abs-path commit-hash local-branch upstream-branch
						      remote-name remote-url repo-state index-num staged-num unstaged-num
						      conflict-num untrack-num commit-ahead-num commit-behind-num stash-num
						      last-tag unstaged-deleted-num staged-new-num staged-deleted-num push-remote-name
						      push-remote-url push-commit-ahead-num push-commit-behind-num skip-worktree-num
						      assume-unchanged-num commit-msg-encoding commit-msg-par))
	       (:copier nil))
  "Gitstatusd response."
  (req-id nil :documentation "Request id. The same as the first field in the request.")
  (is-git-repo nil :documentation "1 - if git repo. 0 - otherwise, all the following is missing.")
  (abs-path nil :documentation "Absolute path to the git repository workdir.")
  (commit-hash nil :documentation "Commit hash that HEAD is pointing to. 40 hex digits.")
  (local-branch nil :documentation "Local branch name or empty if not on a branch.")
  (upstream-branch nil :documentation "Upstream branch name. Can be empty.")
  (remote-name nil :documentation "The remote name, e.g. `upstream' or `origin'.")
  (remote-url nil :documentation " Remote URL. Can be empty.")
  (repo-state nil :documentation "Repository state, A.K.A. action. Can be empty.")
  (index-num nil :documentation "The number of files in the index.")
  (staged-num nil :documentation "The number of staged changes.")
  (unstaged-num nil :documentation "The number of unstaged changes.")
  (conflict-num nil :documentation "The number of conflicted changes.")
  (untrack-num nil :documentation "The number of untracked files.")
  (commit-ahead-num nil :documentation "Number of commits the current branch is ahead of upstream.")
  (commit-behind-num nil :documentation "Number of commits the current branch is behind upstream.")
  (stash-num nil :documentation "The number of stashes.")
  (last-tag nil :documentation "The last tag (in lexicographical order) that points to the same commit as HEAD.")
  (unstaged-deleted-num nil :documentation "The number of unstaged deleted files.")
  (staged-new-num nil :documentation "The number of staged new files.")
  (staged-deleted-num nil :documentation "The number of staged deleted files.")
  (push-remote-name nil :documentation " The push remote name, e.g. `upstream' or `origin'.")
  (push-remote-url nil :documentation "Push remote URL. Can be empty.")
  (push-commit-ahead-num nil :documentation "Number of commits the current branch is ahead of push remote.")
  (push-commit-behind-num nil :documentation "Number of commits the current branch is behind push remote.")
  (skip-worktree-num nil :documentation "Number of files in the index with skip-worktree bit set.")
  (assume-unchanged-num nil :documentation "Number of files in the index with assume-unchanged bit set.")
  (commit-msg-encoding nil :documentation "Encoding of the HEAD's commit message. Empty value means UTF-8.")
  (commit-msg-par nil :documentation "The first paragraph of the HEAD's commit message as one line."))

(defun gitstatusd--filter (_ str)
  "Filter `Gitstatusd' STR output."
  (when gitstatusd-callback
    ;; TODO split by rec sep
    (let ((proc (apply #'gitstatusd-create (split-string str gitstatusd--unit-sep))))
      (funcall gitstatusd-callback proc))))

(defun make--gitstatusd-process ()
  "Create `Gitstatusd' process if it doesn't exist."
  (unless gitstatusd--proc
    (let* ((cmd-args (split-string gitstatusd-cmd-args))
	   (proc (push (expand-file-name gitstatusd-exe) cmd-args)))
      (setq gitstatusd--proc
	    (make-process
	     :name "gitstatusd"
	     :buffer "gitstatusd"
	     :connection-type 'pipe
	     :filter #'gitstatusd--filter
	     :command proc))))
  gitstatusd--proc)

;;;###autoload
(defun gitstatusd-get-status (path)
  "Make asynchronous request to `gitstatusd' for PATH.

Immediately return the request ID."
  ;; TODO what if file is sent
  (let* ((dir (expand-file-name path))
	 (req-id (concat
		  (file-name-nondirectory (directory-file-name dir)) "-"
		  (number-to-string (time-convert nil 'integer))))
	 (record (concat req-id gitstatusd--unit-sep dir
			 gitstatusd--unit-sep
			 (if gitstatusd-compute-by-index "0" : "1")
			 gitstatusd--record-sep)))
    (process-send-string (make--gitstatusd-process) record)
    req-id))

(provide 'gitstatusd)
;;; gitstatusd.el ends here

