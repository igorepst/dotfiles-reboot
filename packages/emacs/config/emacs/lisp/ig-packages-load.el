;;; ig-packages-load.el --- Package.el config  -*- lexical-binding: t; -*-

;;; Commentary:
;; Package.el config

;;; Code:

(require 'package)
(push '("melpa" . "https://melpa.org/packages/") package-archives)
(setq package-user-dir (concat ig-cache-dir "packages/install")
      package-native-compile t
      package-quickstart t
      package-quickstart-file (concat ig-cache-dir "packages/package-quickstart.el")
      package-gnupghome-dir (concat ig-cache-dir "packages/gnupg"))
;; Holds package-selected-packages that is autogenerated
(load (concat ig-cache-dir "packages/selected-packages") t t nil)
(package-activate-all)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
(provide 'ig-packages-load)
;;; ig-packages-load.el ends here
