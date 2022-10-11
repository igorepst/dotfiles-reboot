;;; gitstatus-eshell.el --- `eshell' front-end for `gitstatusd' -*- lexical-binding: t; -*-

;;; Commentary:
;; `eshell' front-end for `gitstatusd'

;;; Code:

(require 'gitstatus)
(require 'em-prompt)

(defgroup gitstatus-eshell nil
  "`eshell' front-end for `gitstatusd'."
  :group 'gitstatus)

(defcustom gitstatus-eshell-neighbour-regex "\\($\\)"
  "Neighbour of the `gitstatus' in `eshell' prompt."
  :type '(string)
  :group 'gitstatus-eshell)

(defcustom is-gitstatus-eshell-neighbour-append nil
  "Whether to append (or prepend) the `gitstatus' to the `eshell' prompt."
  :type '(string)
  :group 'gitstatus-eshell)

(defcustom gitstatus-eshell-prompt-lines 1
  "Search for `gitstatus-eshell-neighbour-regex' in this many lines."
  :type '(integer)
  :group 'gitstatus-eshell)

;; TODO find right buffer to change
;;;###autoload
(defun gitstatus-eshell-build (res)
  "Build `eshell' prompt based on `gitstatusd' result, represented by RES."
  (let ((msg (gitstatus-build-str res)))
    (when (gitstatus--string-not-empty-p msg)
      (save-excursion
	(save-match-data
	  (let ((place (gitstatus--eshell-find-place)))
	    (when place
	      (forward-char place)
	      (let* ((pos (point))
		     (inhibit-read-only t))
		(insert (concat " " msg))
		(add-text-properties pos (+ 1 pos (length msg))
				     '(read-only t
						 front-sticky (read-only)
						 rear-nonsticky (read-only)))))))))))

(defun gitstatus--eshell-find-place ()
  "Find the right place in `eshell' prompt."
  (goto-char (point-max))
  (re-search-backward eshell-prompt-regexp nil t 1)
  (let ((cnt gitstatus-eshell-prompt-lines)
	(mstart (match-beginning 0))
	(place))
    (when (and mstart (> cnt 0))
      (while (> cnt 0)
	(let* ((start (if (= cnt gitstatus-eshell-prompt-lines) mstart (line-beginning-position)))
	       (end (if (= cnt gitstatus-eshell-prompt-lines) (match-end 0) (line-end-position)))
	       (str (buffer-substring start end)))
	  (string-match gitstatus-eshell-neighbour-regex str)
	  (setq place
		(if is-gitstatus-eshell-neighbour-append
		    (match-end 1)
		  (match-beginning 1)))
	  (if place
	      (setq cnt 0)
	    (forward-line -1)
	    (beginning-of-line)
	    (setq cnt (1- cnt))))))
    place))

(provide 'gitstatus-eshell)
;;; gitstatus-eshell.el ends here
