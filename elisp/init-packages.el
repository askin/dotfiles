;;; init-packages.el - setup dependent elisp packages via el-get and elpa
;;
;; based on Steve Purcell's init-el-get.el


;; start ELPA
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             'APPEND)
(package-initialize)

;; ;; Local sources - recipes not included with el-get
;; (setq el-get-sources
;;       '(
;;         (:name hungry-delete :type elpa)
;;         (:name eproject
;;                :type git :url "http://github.com/jrockway/eproject.git"
;;                :features (eproject eproject-extras))
;;         (:name idle-highlight-mode :type elpa)
;;         (:name highlight-parentheses :type elpa)
;;         (:name dlacewell-minimap
;;                :type git :url "https://github.com/dustinlacewell/emacs-minimap.git"
;;                :features minimap)))

(setq packages
      '(
        linum-off
        ))

(provide 'init-packages)
