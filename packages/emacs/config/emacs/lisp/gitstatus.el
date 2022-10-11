;;; gitstatus.el --- Front-end for `gitstatusd' -*- lexical-binding: t; -*-

;;; Commentary:
;; Front-end for `gitstatusd'

(require 'gitstatusd)
(require 'em-prompt)

(defgroup gitstatus nil
  "Front-end for `gitstatusd'."
  :group 'gitstatusd)

(defcustom gitstatus-eshell-neighbor-regex "\\($\\)"
  "Neighbor of the gitstatus in `eshell' prompt."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-eshell-neighbor-append nil
  "Whether to append (or prepend) the gitstatus to the `eshell' prompt."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-eshell-prompt-lines 1
  "Search for `gitstatus-eshell-neighbor-regex' in this many lines."
  :type '(integer)
  :group 'gitstatus)

(defcustom gitstatus-prefix nil
  "Prefix to prepend to the gitstatus."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-suffix nil
  "Suffix to append to the gitstatus."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-branch-truncate-after 30
  "Truncate branch name after this much characters."
  :type '(integer)
  :group 'gitstatus)

(defcustom gitstatus-branch-truncation-sep "…"
  "In case the branch name is truncated, use this as a separator."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-branch-icon ""
  "Icon for the branch."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-tag-icon "@"
  "Icon for the tag."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-hash-icon "#"
  "Icon for the hash."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-upstream-sep ":"
  "Separator netween local and upstream branch."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-commit-behind-icon "⇣"
  "Icon for the number of commits the current branch is behind upstream."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-commit-ahead-icon "⇡"
  "Icon for the number of commits the current branch is ahead of upstream."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-push-commit-behind-icon "⇠"
  "Icon for the number of commits the current branch is behind push remote."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-push-commit-ahead-icon "⇢"
  "Icon for the number of commits the current branch is ahead of push-remote."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-stash-icon "*"
  "Icon for the number of stashes."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-conflict-icon "~"
  "Icon for the number of conflicted changes."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-staged-icon "+"
  "Icon for the number of staged changes."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-unstaged-icon "!"
  "Icon for the number of unstaged changes."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-untracked-icon "?"
  "Icon for the number of untracked files."
  :type '(string)
  :group 'gitstatus)

(defcustom gitstatus-unstaged-unknown-icon "─"
  "Icon when the number of unstaged changes is unknown."
  :type '(string)
  :group 'gitstatus)

(defgroup gitstatus-faces nil
  "Faces used by gitstatus."
  :group 'gitstatus
  :group 'faces)

(defface gitstatus-default-face
  '((t (:inherit default)))
  "Default face for gitstatus."
  :group 'gitstatus-faces)

(defface gitstatus-clean-face
  '((t (:inherit success)))
  "Clean face for gitstatus."
  :group 'gitstatus-faces)

(defface gitstatus-modified-face
  '((t (:inherit font-lock-constant-face)))
  "Modified face for gitstatus."
  :group 'gitstatus-faces)

(defface gitstatus-untracked-face
  '((t (:inherit font-lock-comment-face)))
  "Untracked face for gitstatus."
  :group 'gitstatus-faces)

(defface gitstatus-conflicted-face
  '((t (:inherit warning)))
  "Conflicted face for gitstatus."
  :group 'gitstatus-faces)

(setq gitstatusd-callback #'gitstatus-build-eshell)
(defvar-local gitstatusd--req-id nil "`gitstatusd' request ID.")

;;;###autoload
(defun gitstatus-start ()
  "Run `gitstatusd' to get the gitstatus information."
  (setq gitstatusd--req-id
	(gitstatusd-get-status default-directory)))

;; TODO find right buffer and place to change
;; TODO customize truncation
(defun gitstatus-build-eshell (res)
  "Build `eshell' prompt based on `gitstatusd' result, represented by RES."
  (let ((msg (gitstatus-build-str res)))
    (when (gitstatus--string-not-empty-p msg)
      (save-excursion
	(save-match-data
	  (goto-char (point-max))
	  (re-search-backward eshell-prompt-regexp nil t 1)
	  (forward-line -1)
	  (beginning-of-line)
	  (let ((place nil)
		(str (buffer-substring (line-beginning-position) (line-end-position))))
	    (string-match gitstatus-eshell-neighbor-regex str)
	    (setq place
		  (if gitstatus-eshell-neighbor-append
		      (match-end 1)
		    (match-beginning 1)))
	    (when place
	      (forward-char place)
	      (let* ((pos (point))
		     (inhibit-read-only t))
		(insert " " msg)
		(add-text-properties pos (+ 1 pos (length msg))
				     '(read-only t
						 front-sticky (read-only)
						 rear-nonsticky (read-only)))))))))))

(defmacro gitstatus--push-prop (val sym msgl face)
  "Helper to add SYM and VAL to MSGL.

Propertize with FACE."
  `(when (gitstatus--has-counter ,val)
     (let ((msg (propertize (concat ,sym ,val) 'face ,face)))
       (push msg ,msgl))))

(defun gitstatus-build-str (res)
  "Build gitstatus string from RES."
  (when (and (string= "1" (gitstatusd-is-git-repo res))
	     (string= gitstatusd--req-id (gitstatusd-req-id res)))
    (let ((branch (gitstatus--get-branch-name res)))
      (let ((case-fold-search nil)
	    (wip (string-match "[^[:alnum:]]\*\\(wip\\|WIP\\)[^[:alnum:]]\*" (gitstatusd-commit-msg-par res))))
	(let ((msgl-s (mapconcat 'identity (reverse (gitstatus--get-counters res)) " ")))
	  (concat
	   (when (gitstatus--string-not-empty-p gitstatus-prefix) (propertize gitstatus-prefix 'face 'gitstatus-default-face))
	   (when (gitstatus--string-not-empty-p branch) (concat " " branch))
	   (when wip (concat " " (propertize "wip" 'face 'gitstatus-modified-face)))
	   (when (gitstatus--string-not-empty-p msgl-s) (concat " " msgl-s))
	   (when (gitstatus--string-not-empty-p gitstatus-suffix) (propertize gitstatus-suffix 'face 'gitstatus-default-face))))))))

(defun gitstatus--get-counters (res)
  "Get counters according to RES."
  (let ((msgl '())
	(unstaged (gitstatusd-unstaged-num res)))
    (gitstatus--push-prop (gitstatusd-commit-behind-num res) gitstatus-commit-behind-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-commit-ahead-num res) gitstatus-commit-ahead-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-push-commit-behind-num res) gitstatus-push-commit-behind-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-push-commit-ahead-num res) gitstatus-push-commit-ahead-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-stash-num res) gitstatus-stash-icon msgl 'gitstatus-clean-face)
    (gitstatus--push-prop (gitstatusd-repo-state res) nil msgl 'gitstatus-conflicted-face)
    (gitstatus--push-prop (gitstatusd-conflict-num res) gitstatus-conflict-icon msgl 'gitstatus-conflicted-face)
    (gitstatus--push-prop (gitstatusd-staged-num res) gitstatus-staged-icon msgl 'gitstatus-modified-face)
    (gitstatus--push-prop unstaged gitstatus-unstaged-icon msgl 'gitstatus-modified-face)
    (gitstatus--push-prop (gitstatusd-untrack-num res) gitstatus-untracked-icon msgl 'gitstatus-untracked-face)
    (when (string= "-1" unstaged)
      (push (propertize gitstatus-unstaged-unknown-icon 'face 'gitstatus-modified-face) msgl))
    msgl))

(defun gitstatus--get-branch-name (res)
  "Get branch name according to RES."
  (let ((branch (gitstatusd-local-branch res))
	(up-branch (gitstatusd-upstream-branch res))
	(ret))
    (if (gitstatus--string-not-empty-p branch)
	(setq ret
	      (propertize
	       (concat gitstatus-branch-icon " " (gitstatus--branch-truncate branch))
	       'face 'gitstatus-clean-face))
      (setq ret (gitstatusd-last-tag res))
      (if (gitstatus--string-not-empty-p ret)
	  (setq ret
		(concat
		 (propertize gitstatus-tag-icon 'face 'gitstatus-default-face)
		 (propertize (gitstatus--branch-truncate branch) 'face 'gitstatus-clean-face)))
	(setq ret (gitstatusd-commit-hash res))
	(setq ret
	      (concat
	       (propertize gitstatus-hash-icon 'face 'gitstatus-default-face)
	       (propertize (substring branch 0 7) 'face 'gitstatus-clean-face)))))
    (when (and (gitstatus--string-not-empty-p up-branch) (not (string= up-branch branch)))
      (setq ret (concat ret
			(propertize gitstatus-upstream-sep 'face 'gitstatus-default-face)
			(propertize (gitstatus--branch-truncate up-branch) 'face 'gitstatus-clean-face))))
    ret))

(defun gitstatus--branch-truncate (branch)
  "Truncate the name of the BRANCH."
  (if (length< branch gitstatus-branch-truncate-after)
      branch
    (let ((end
	   (if (> 15 gitstatus-branch-truncate-after) 3
	     (if (> 30 gitstatus-branch-truncate-after) 7 10))))
      (concat (substring branch 0 end) gitstatus-branch-truncation-sep (substring branch (- 0 end))))))

(defun gitstatus--string-not-empty-p (string)
  "Check whether STRING is not null and not empty."
  (not (or (null string) (string= string ""))))

(defun gitstatus--has-counter (val)
  "Check whether the VAL exists."
  (and (gitstatus--string-not-empty-p val) (not (string= "0" val))))

;;; Code:
(provide 'gitstatus)
;;; gitstatus.el ends here
