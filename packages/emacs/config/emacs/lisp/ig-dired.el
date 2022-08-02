;;; ig-dired.el --- Dired configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of Dired.

;;; Code:

(require 'ig-common)

(defconst ig-ls-switches "-Alh --group-directories-first --time-style \"+%d-%m-%Y %H:%M\"" "'ls' switches.")

(setq dired-use-ls-dired t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-listing-switches (purecopy ig-ls-switches))

(defun ig-dired-sort-helper (variant &optional reverse)
  "Sort Dired by VARIANT, possibly in REVERSE order.
The sorting mode will be used from now on."
  (let ((switches (concat (purecopy ig-ls-switches) " " variant)))
    (when reverse (setq switches (concat "--reverse " switches)))
    (setq dired-listing-switches switches)
    (when (eq major-mode 'dired-mode)
      (dired-sort-other switches))))

;;;###autoload
(defun ig-dired-sort (sort-order)
  "Sort Dired according to SORT-ORDER."
  (interactive (list (alt-completing-read "Sort order: "
					  '(("Name" . ("-v" . nil))
					    ("Name reversed" . ("-v" . t))
					    ("Time" . ("-t" . t))
					    ("Time reversed" . ("-t" . nil))
					    ("Size" . ("-S" . t))
					    ("Size reversed" . ("-S" . nil))
					    ("Ext" . ("-X" . nil))
					    ("Ext reversed" . ("-X" . t))) t)))
  (ig-dired-sort-helper (car sort-order) (cdr sort-order)))

(defun ig-dired-sort-set-mode-line (_args)
  "Override mode name."
  (let* ((asc t) (name (cond ((string-match-p
				"-v$" dired-actual-switches)
			       "Dir name")
			      ((string-match-p
				"-t$" dired-actual-switches)
			       (setq asc nil)
			       "Dir time")
			      ((string-match-p
				"-S$" dired-actual-switches)
			       (setq asc nil)
			       "Dir size")
			      ((string-match-p
				"-X$" dired-actual-switches)
			       "Dir ext")
			      (t
			       (concat "Dired " dired-actual-switches)))))
       (setq mode-name
	     (concat name
		     (if (string-match-p "^--reverse" dired-actual-switches)
			 (if asc " ↑" " ↓") (if asc " ↓" " ↑"))))
       (force-mode-line-update)))
(advice-add 'dired-sort-set-mode-line :around #'ig-dired-sort-set-mode-line)

(ig-dired-sort-helper "-v" nil)

;; LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=34;42:st=37;44:ex=01;32

(defgroup ig-dired-faces nil
  "Faces used by Dired."
  :group 'dired
  :group 'faces)

(defface ig-dired-video-face
  `((t :foreground ,ig-color-magenta))
  "Face for Dired video files."
  :group 'ig-dired-faces)

(defface ig-dired-archive-face
  `((t :foreground ,ig-color-red))
  "Face for Dired archive files."
  :group 'ig-dired-faces)

(defface ig-dired-img-face
  `((t :foreground ,ig-color-yellow))
  "Face for Dired image files."
  :group 'ig-dired-faces)

(defface ig-dired-audio-face
  `((t :foreground ,ig-color-cyan))
  "Face for Dired image files."
  :group 'ig-dired-faces)

;; "mp4" "mp4v" "mkv" "mpg" "mpeg" "webm" "webp" "vob" "wmv" "avi" "ts" "mts" "vid"
(add-to-list 'dired-font-lock-keywords (list "\\(?:\\.\\(?:avi\\|m\\(?:kv\\|p\\(?:4v\\|eg\\|[4g]\\)\\|ts\\)\\|ts\\|v\\(?:id\\|ob\\)\\|w\\(?:eb[mp]\\|mv\\)\\)\\)$" '(".+" (dired-move-to-filename) nil (0 'ig-dired-video-face))) t)

;; "tar" "tgz" "lzma" "zip" "xz" "zst" "bz2" "bz" "deb" "rpm" "jar" "war" "rar" "cpio" "7z" "cab"
(add-to-list 'dired-font-lock-keywords (list "\\(?:\\.\\(?:7z\\|bz2?\\|c\\(?:ab\\|pio\\)\\|deb\\|jar\\|lzma\\|r\\(?:ar\\|pm\\)\\|t\\(?:ar\\|gz\\)\\|war\\|xz\\|z\\(?:ip\\|st\\)\\)\\)$" '(".+" (dired-move-to-filename) nil (0 'ig-dired-archive-face))) t)

;; "jpg" "jpeg" "gif" "bmp" "pbm" "pgm" "ppm" "tga" "xbm" "xpm" "tif" "tiff" "png" "svg" "svgz" "pcx" "xcf"
(add-to-list 'dired-font-lock-keywords (list "\\(?:\\.\\(?:bmp\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|cx\\|gm\\|ng\\|pm\\)\\|svgz?\\|t\\(?:ga\\|iff?\\)\\|x\\(?:bm\\|cf\\|pm\\)\\)\\)$" '(".+" (dired-move-to-filename) nil (0 'ig-dired-img-face))) t)

;; "flac" "midi" "mka" "mp3" "ogg" "wav" "oga" "opus" "spx"
(add-to-list 'dired-font-lock-keywords (list "\\(?:\\.\\(?:flac\\|m\\(?:idi\\|ka\\|p3\\)\\|o\\(?:g[ag]\\|pus\\)\\|spx\\|wav\\)\\)$" '(".+" (dired-move-to-filename) nil (0 'ig-dired-audio-face))) t)

;; "bak" "old" "orig" "part" "swp" "tmp" "dpkg-dist" "dpkg-old" "rpmnew" "rpmorig" "rpmsave" "pacnew" "pacsave"
(add-to-list 'dired-font-lock-keywords (list "\\(?:\\.\\(?:bak\\|dpkg-\\(?:dist\\|old\\)\\|o\\(?:ld\\|rig\\)\\|pa\\(?:c\\(?:new\\|save\\)\\|rt\\)\\|rpm\\(?:new\\|orig\\|save\\)\\|\\(?:sw\\|tm\\)p\\)\\)$" '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))) t)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-dired)
;;; ig-dired.el ends here
