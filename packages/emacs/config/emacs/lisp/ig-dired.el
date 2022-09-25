;;; ig-dired.el --- Dired configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration of Dired.

;;; Code:

(require 'ig-common)

(defconst ig-ls-switches "-AlhgG --group-directories-first" "'ls' switches.")

(setq dired-use-ls-dired t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-dwim-target t
      dired-listing-switches (concat ig-ls-switches " -v")
      dired-switches-in-mode-line #'ig-dired-sort-set-mode-line
      dired-isearch-filenames t)

(defun ig-dired-sort-helper (variant &optional reverse)
  "Sort Dired by VARIANT, possibly in REVERSE order.
The sorting mode will be used from now on."
  (let ((switches (concat ig-ls-switches " " variant)))
    (when reverse (setq switches (concat "--reverse " switches)))
    (setq dired-listing-switches switches)
    (when (eq major-mode 'dired-mode)
      (dired-sort-other switches))))

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
  (let ((so (car sort-order)))
    (when (and so (string-match-p "\\(?:-[SXtv]\\)$" so))
      (ig-dired-sort-helper so (cdr sort-order)))))

(defun ig-dired-sort-set-mode-line (das)
  "Override mode name according to DAS - `dired-actual-switches'."
  (let* ((asc t) (name (cond ((string-match-p "-v$" das) "name")
			     ((string-match-p "-t$" das)
			      (setq asc nil) "time")
			     ((string-match-p "-S$" das)
			      (setq asc nil) "size")
			     ((string-match-p "-X$" das) "ext")
			     (t das))))
    (concat name
	    (if (string-match-p "^--reverse" das)
		(if asc " ↑" " ↓") (if asc " ↓" " ↑")))))

;; "mp4" "mp4v" "mkv" "mpg" "mpeg" "webm" "webp" "vob" "wmv" "avi" "ts" "mts" "vid" "flac" "midi" "mka" "mp3" "ogg" "wav" "oga" "opus" "spx"
(defconst ig-dired-media-ext "\\(?:\\.\\(?:avi\\|flac\\|m\\(?:idi\\|k[av]\\|p\\(?:4v\\|eg\\|[34g]\\)\\|ts\\)\\|o\\(?:g[ag]\\|pus\\)\\|spx\\|ts\\|v\\(?:id\\|ob\\)\\|w\\(?:av\\|eb[mp]\\|mv\\)\\)\\)$" "Dired media files extensions.")

(require 'dired-x)
(setq dired-clean-confirm-killing-deleted-buffers nil
      dired-guess-shell-alist-user
      `((,ig-dired-media-ext "mpv")
	("\\.\\(?:pdf\\|djvu\\)\\'" "evince")))

;; TODO add this
;; LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=34;42:st=37;44:ex=01;32

(defgroup ig-dired-faces nil
  "Faces used by Dired."
  :group 'dired
  :group 'faces)

(defface ig-dired-media-face
  `((t :foreground ,ig-color-magenta))
  "Face for Dired media files."
  :group 'ig-dired-faces)

(defface ig-dired-archive-face
  `((t :foreground ,ig-color-red))
  "Face for Dired archive files."
  :group 'ig-dired-faces)

(defface ig-dired-img-face
  `((t :foreground ,ig-color-yellow))
  "Face for Dired image files."
  :group 'ig-dired-faces)

(defface ig-dired-doc-face
  `((t :foreground ,ig-color-cyan))
  "Face for Dired document files."
  :group 'ig-dired-faces)

(defface ig-dired-exe-face
  `((t :foreground ,ig-color-green))
  "Face for Dired executable files."
  :group 'ig-dired-faces)

(defconst ig-dired-font-lock-keywords '() "Dired font lock additional keywords.")

(push (list (concat dired-re-maybe-mark dired-re-inode-size "[-d]........\\(x\\)") '(".+" (dired-move-to-filename) nil (0 'ig-dired-exe-face))) ig-dired-font-lock-keywords)

(push (list ig-dired-media-ext '(".+" (dired-move-to-filename) nil (0 'ig-dired-media-face))) ig-dired-font-lock-keywords)

;; "tar" "tgz" "lzma" "zip" "xz" "zst" "bz2" "bz" "deb" "rpm" "jar" "war" "rar" "cpio" "7z" "cab" "gz" "iso" "apk"
(push (list "\\(?:\\.\\(?:7z\\|apk\\|bz2?\\|c\\(?:ab\\|pio\\)\\|deb\\|gz\\|iso\\|jar\\|lzma\\|r\\(?:ar\\|pm\\)\\|t\\(?:ar\\|gz\\)\\|war\\|xz\\|z\\(?:ip\\|st\\)\\)\\)$" '(".+" (dired-move-to-filename) nil (0 'ig-dired-archive-face))) ig-dired-font-lock-keywords)

;; "jpg" "jpeg" "gif" "bmp" "pbm" "pgm" "ppm" "tga" "xbm" "xpm" "tif" "tiff" "png" "svg" "svgz" "ico" "xcf" "psd"
(push (list "\\(?:\\.\\(?:bmp\\|gif\\|ico\\|jp\\(?:e?g\\)\\|p\\(?:bm\\|gm\\|ng\\|pm\\|sd\\)\\|svgz?\\|t\\(?:ga\\|iff?\\)\\|x\\(?:bm\\|cf\\|pm\\)\\)\\)$" '(".+" (dired-move-to-filename) nil (0 'ig-dired-img-face))) ig-dired-font-lock-keywords)

;; "doc" "docx" "odt" "xls" "xlsx" "ods" "pdf" "djvu"
(push (list "\\(?:\\.\\(?:d\\(?:jvu\\|ocx?\\)\\|od[st]\\|pdf\\|xlsx?\\)\\)$" '(".+" (dired-move-to-filename) nil (0 'ig-dired-doc-face))) ig-dired-font-lock-keywords)

;; "bak" "old" "orig" "part" "swp" "tmp" "dpkg-dist" "dpkg-old" "rpmnew" "rpmorig" "rpmsave" "pacnew" "pacsave" "eln"
(push (list "\\(?:\\.\\(?:bak\\|dpkg-\\(?:dist\\|old\\)\\|eln\\|o\\(?:ld\\|rig\\)\\|pa\\(?:c\\(?:new\\|save\\)\\|rt\\)\\|rpm\\(?:new\\|orig\\|save\\)\\|\\(?:sw\\|tm\\)p\\)\\)$" '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))) ig-dired-font-lock-keywords)

(nconc dired-font-lock-keywords (nreverse ig-dired-font-lock-keywords))
(makunbound 'ig-dired-sort-font-lock-keywords)

(define-key dired-mode-map "r" #'ig-dired-run-proc-async-nohup)
(define-key dired-mode-map "'" #'ig-eshell-switch-or-new)
(define-key dired-mode-map [M-up] #'dired-up-directory)
(define-key dired-mode-map "/" #'dired-isearch-filenames)
(define-key dired-mode-map "z" #'ig-dired-get-size)
(define-key dired-mode-map "w" #'ig-dired-copy-filename-as-kill)
(define-key dired-mode-map "e" #'ora-ediff-files)

;; This defines LEFT click. For the explanation of mouse-1 vs mouse-2 in Dired buffers, see https://emacs.stackexchange.com/a/36330/2477
(define-key dired-mode-map [mouse-2] #'dired-mouse-find-file)
(define-key dired-mode-map [C-mouse-1] #'mouse-set-point)
(define-key dired-mode-map [C-down-mouse-1] #'dired-mouse-find-file-other-window)
(define-key dired-mode-map [M-mouse-1] #'mouse-set-point)
(define-key dired-mode-map [M-down-mouse-1] #'dired-up-directory)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'ig-dired)
;;; ig-dired.el ends here
