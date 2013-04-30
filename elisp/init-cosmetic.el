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
(setq-default
 mode-line-format
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

;; Show line numbers
(global-linum-mode 1)

(line-number-mode t)                     ;; show line numbers
(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

;; Emacs Window Geometry
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 130))

;; Font Size
(set-face-attribute 'default nil :height 85)

;; load deviant theme
(load-file "~/.elisp/deviant-theme.el")

;; (set-face-foreground 'mode-line "red")

(provide 'init-cosmetic)
