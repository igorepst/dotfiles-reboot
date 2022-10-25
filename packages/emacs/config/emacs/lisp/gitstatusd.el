;;; gitstatusd.el --- Wrapper for gitstatusd -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Igor Epstein

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The wrapper around an external gitstatusd executable.
;; Errors are written into `*gitstatusd err*' buffer

;;; Code:

(require 'cl-lib)


;;; Customizable variables

(defgroup gitstatusd nil
  "Emacs interface to gitstatusd."
  :group 'tools)

(defcustom gitstatusd-exe "gitstatusd"
  "Gitstatusd executable path."
  :type 'file
  :group 'gitstatusd)

(defcustom gitstatusd-cmd-args
  (concat "-s -1 -u -1 -d -1 -c -1 -m -1 -v FATAL -t "
	  (format "%s" (* 2
			  (if (fboundp 'num-processors) (num-processors) 1))))
  "Gitstatusd command line arguments."
  :type 'string
  :group 'gitstatusd)

(defcustom gitstatusd-is-compute-by-index t
  "Whether to perform computations by reading Git index."
  :type 'boolean
  :group 'gitstatusd)


;;; Internal variables

(defvar gitstatusd--proc nil "Gitstatusd process.")

(defvar gitstatusd--callbacks nil "Gitstatusd callbacks structure.")

(defconst gitstatusd--record-sep "" "Record separator.")

(defconst gitstatusd--unit-sep "" "Unit separator.")


;;; Public interface

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

;;;###autoload
(defun gitstatusd-get-status (path callback)
  "Make asynchronous request to gitstatusd for PATH with CALLBACK.

Immediately return the request ID."
  (let ((proc (gitstatusd--make-process)))
    ;; As gitstatusd is a daemon we assume that when the process dies,
    ;; its output is either an error or a garbage
    (when (process-live-p proc)
      (let* ((dir (expand-file-name path))
	     (req-id (concat
		      (file-name-nondirectory (directory-file-name dir)) "-"
		      (mapconcat #'number-to-string (current-time) "")))
	     (rec (concat req-id gitstatusd--unit-sep dir
			  gitstatusd--unit-sep
			  (if gitstatusd-is-compute-by-index "0" : "1")
			  gitstatusd--record-sep)))
	(when callback
	  (unless gitstatusd--callbacks
	    (setq gitstatusd--callbacks (make-hash-table :test 'equal)))
	  (puthash req-id callback gitstatusd--callbacks))
	(process-send-string (gitstatusd--make-process) rec)
	req-id))))

(defun gitstatusd-kill ()
  "Kill gitstatusd process and clear callbacks.

They will be recreated on the next request."
  (interactive)
  (when gitstatusd--proc
    (delete-process gitstatusd--proc)
    (setq gitstatusd--proc nil))
  (gitstatusd--clear-callbacks))


;;; Utility functions

(defun gitstatusd--make-process ()
  "Create gitstatusd process if it doesn't exist."
  (unless gitstatusd--proc
    (gitstatusd--clear-callbacks)
    (let* ((cmd-args (split-string gitstatusd-cmd-args))
	   (proc (push (expand-file-name gitstatusd-exe) cmd-args)))
      (setq gitstatusd--proc
	    (make-process
	     :name "gitstatusd"
	     :connection-type 'pipe
	     :sentinel #'gitstatusd--sentinel
	     :filter #'gitstatusd--filter
	     :noquery t
	     :command proc))))
  gitstatusd--proc)

(defun gitstatusd--filter (proc str)
  "Filter gitstatusd PROC process's STR output."
  (if (process-live-p proc)
      (dolist (res (split-string str gitstatusd--record-sep t))
	(let* ((proc-out (apply #'gitstatusd-create (split-string res gitstatusd--unit-sep)))
	       (req-id (gitstatusd-req-id proc-out))
	       (gitstatusd-callback (gethash req-id gitstatusd--callbacks)))
	  (when gitstatusd-callback
	    (remhash req-id gitstatusd--callbacks)
	    (funcall gitstatusd-callback proc-out))))
    (gitstatusd--clear-callbacks)
    ;; Create this buffer only in case of an error, as the process' buffer is nil
    (with-current-buffer (get-buffer-create "*gitstatusd err*")
      (when (not (executable-find gitstatusd-exe))
	(insert "Cannot find gitstatusd executable. See https://github.com/romkatv/gitstatus
for the installation instructions and ensure it is in path.\n"))
      (insert str))))

(defun gitstatusd--sentinel (proc _)
  "Process PROC sentinel."
  (when (memq (process-status proc) '(exit signal))
    (setq gitstatusd--proc nil)))

(defun gitstatusd--clear-callbacks ()
  "Clear callbacks structure."
  (when gitstatusd--callbacks
    (clrhash gitstatusd--callbacks)
    (setq gitstatusd--callbacks nil)))

(provide 'gitstatusd)
;;; gitstatusd.el ends here

