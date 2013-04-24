;; Environment
(set-language-environment 'turkish)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Load directory
(setq load-path (cons "~/.elisp" load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; COSMETICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Theme
(require 'color-theme)
(condition-case nil
    (color-theme-initialize)
  (error nil))

;; Disable emacs splash screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Cursor
(setq scroll-conservatively 1)
(setq scroll-step 1)

;; No Bar - No menu
(set-scroll-bar-mode nil)

(tool-bar-mode nil)
(tool-bar-mode -1)

(menu-bar-mode nil)
(menu-bar-mode -1)

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
      (list
       ;; the buffer name; the file name as a tool tip
       '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                           'help-echo (buffer-file-name)))

       ;; line and column
       "(" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%02l" 'face 'font-lock-type-face) ","
       (propertize "%02c" 'face 'font-lock-type-face)
       ") "

       ;; relative position, size of file
       "["
       (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       "/"
       (propertize "%I" 'face 'font-lock-constant-face) ;; size
       "] "

       ;; the current major mode for the buffer.
       "["

       '(:eval (propertize "%m" 'face 'font-lock-string-face
                           'help-echo buffer-file-coding-system))
       "] "


       "[" ;; insert vs overwrite mode, input-method in a tooltip
       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                           'face 'font-lock-preprocessor-face
                           'help-echo (concat "Buffer is in "
                                              (if overwrite-mode "overwrite" "insert") " mode")))

       ;; was this buffer modified since the last save?
       '(:eval (when (buffer-modified-p)
                 (concat ","  (propertize "Mod"
                                          'face 'font-lock-warning-face
                                          'help-echo "Buffer has been modified"))))

       ;; is this buffer read-only?
       '(:eval (when buffer-read-only
                 (concat ","  (propertize "RO"
                                          'face 'font-lock-type-face
                                          'help-echo "Buffer is read-only"))))
       "] "

       ;; add the time, with the date and the emacs uptime in the tooltip
       '(:eval (propertize (format-time-string "%H:%M")
                           'help-echo
                           (concat (format-time-string "%c; ")
                                   (emacs-uptime "Uptime:%hh"))))
       " --"
       ;; i don't want to see minor-modes; but if you want, uncomment this:
       minor-mode-alist  ;; list of minor modes
       "%-" ;; fill with '-'
       )
      )

(set (make-local-variable lisp-indent-function)
     'common-lisp-indent-function)

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "gray60" :background "gray20"
    :inverse-video nil
    :box '(:line-width 2 :color "gray20" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray80" :background "gray40"
    :inverse-video nil
    :box '(:line-width 2 :color "gray40" :style nil))

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

;; Title Format
(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-directory (or (buffer-file-name) default-directory))
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF COSMETICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; YASNIPPET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path
             "~/.elisp/yasnippet")

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
(setq-default indent-tabs-mode nil)

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
(global-set-key (kbd "C-c d") "\C-a\C- \C-n\M-w\C-y")
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
(setq-default fill-column 80)
(setq auto-fill-mode 1)
(setq default-major-mode 'text-mode)
(setq initial-scratch-message nil)

;; Autosave & Backup
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Remove White Spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show line numbers
(global-linum-mode 1)

;; New
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no
(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n")

(setq inhibit-startup-message t          ;; don't show ...
  inhibit-startup-echo-area-message t)   ;; ... startup messages
(setq require-final-newline t)           ;; end files with a newline

(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; some handy packages
;;
;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

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

;; Emacs Window Geometry
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 130))

;; Font Size
(set-face-attribute 'default nil :height 100)

;; activate uppercase - lowercase functions
(global-hl-line-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Auto Complete: http://cx4a.org/software/auto-complete/
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; Fill Column Indicator: https://github.com/alpaker/Fill-Column-Indicator
(require 'fill-column-indicator)
(setq fci-rule-width 5)
(setq fci-rule-color "gray20")

;; User fci-mode for defined modes
(mapc
 (lambda (language-mode-hook)
   (add-hook language-mode-hook 'fci-mode))
 '(python-mode-hook c-mode-hook lisp-mode-hook js-mode-hook))

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

;; load deviant theme
(load-file "~/.elisp/deviant-theme.el")

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

;; Org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Browse the current url
(global-set-key (kbd "C-c u") 'browse-url)
