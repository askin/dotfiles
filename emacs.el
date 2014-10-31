;; Environment
(set-language-environment 'turkish)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Load directory
(setq load-path (cons "~/.elisp" load-path))

;; configurations
;; New
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no
(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n")

(setq inhibit-startup-message t          ;; don't show ...
      inhibit-startup-echo-area-message t)   ;; ... startup messages

(require 'init-cosmetic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; YASNIPPET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path
             "~/.elisp/yasnippet")

(add-to-list 'custom-theme-load-path
             "~/.elisp/emacs-color-theme-solarized")

(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.elisp/yasnippet/snippets")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; YASNIPPET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes
;; Don't use tabs in any text-mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

;; Indentation
(setq standart-indent 4)
(setq-default indent-tabs-mode t)

;; C indent with 4 space
(setq c-default-style "bsd"
      c-basic-offset 4)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)


(add-to-list 'auto-mode-alist '("\\.psp\\'" . html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Keyboard Fixes
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(autoload 'rm-set-mark "rect-mark" "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark
  "rect-mark" "Exchange point and mark for rectangle." t)

;; Disable Ctrl Z
(global-set-key (kbd "C-x C-z") nil)

;; Undo - For KDE
(global-set-key (kbd "C-z") 'undo)

;; Find and Replace
;; (global-set-key (kbd "<f4>") 'replace-string)

;; Auto Complete
(global-set-key (kbd "<f5>") 'hippie-expand)
(global-set-key (kbd "C-c f") 'browse-url-firefox)  ;; Firefox
;; (global-set-key (kbd "C-c s") 'speedbar)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-c r") 'replace-string)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; show whitespaces at the end of the line
(setq show-trailing-whitespace t)

;; Remote Emacs
(require 'tramp)

;; Scrolling
(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)
(column-number-mode t)

;; Major Mode Customization
(setq auto-fill-mode 1)
(setq default-major-mode 'text-mode)
(setq initial-scratch-message nil)

;; Autosave & Backup
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; use local directory to backup with tramp
(setq tramp-backup-directory-alist backup-directory-alist)

;; Remove White Spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq require-final-newline t)           ;; end files with a newline

;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

;; overrride the default function....
(defun emacs-session-filename (SESSION-ID)
  (concat "~/.emacs.d/cache/session." SESSION-ID))

;; bookmarks
(setq bookmark-default-file "~/.emacs.d/data/bookmarks" ;; bookmarks
  bookmark-save-flag 1)                            ;; autosave each change

;; savehist: save some history
(setq savehist-additional-variables    ;; also save...
  '(search ring regexp-search-ring)    ;; ... my search entries
  savehist-autosave-interval 60        ;; save every minute (default: 5 min)
  savehist-file "~/.emacs.d/cache/savehist")   ;; keep my home clean
(savehist-mode t)                      ;; do customization before activation

;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *")
(setq
  tramp-default-method "ssh"
  tramp-persistency-file-name "~/.emacs.d/cache/tramp")

;; turkish asciify && deasciify
(load-file "~/.elisp/turkish.el")

;; activate uppercase - lowercase functions
(global-hl-line-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Auto Complete: http://cx4a.org/software/auto-complete/
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; Python lambda-mode
(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

;; Python pep8 hooks
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key  (kbd "C-c p") 'pep8)))

;; switch between source and header
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; Marmalade repo
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Php Modes
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Kill all buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; open .md and .markdown files with markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; Post current buffer to gist and browse it (require gist)
(defun gist-buffer-private-browse ()
  (interactive)
  (let ((gist-view-gist t))
    (gist-region-private (point-min) (point-max))))

(global-set-key (kbd "C-c b") 'gist-buffer-private-browse)

;; identica mode
(setq load-path (cons "~/.elisp/identica-mode" load-path))
(require 'identica-mode)
(setq identica-username "kelebek")

;; Browse the current url
(global-set-key (kbd "C-c u") 'browse-url)

;; add fill-column-indicator (fci-mode)
(require 'init-fill-column-indicator)

(require 'init-org-mode)

(setq initial-scratch-message
      (with-temp-buffer
        (insert-file-contents "~/.elisp/ascii.txt")
        (buffer-string)))

(electric-pair-mode 1)

;; VoiceXML Mode
(add-to-list 'auto-mode-alist '("\\.vxml\\'" . nxml-mode))

;; Puppet Mode
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

;; init duplicate lines
(require 'init-duplicate)

(setq puppet-indent-levet 4)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/data/bookmarks"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'init-macros)
