(require 'package)
(require 'cl)
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

; System
(when window-system
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(add-hook 'server-switch-hook (lambda ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))
(add-to-list 'auto-mode-alist (cons (file-truename "~/.emacs") 'emacs-lisp-mode))
(defadvice shell (around always-new-shell)
  (let ((buffer (generate-new-buffer-name "*shell*"))) ad-do-it))
(ad-activate 'shell)
(setq compile-command "")

; minor mode to override keys globally
(defvar my-keys-mode-map (make-keymap) "my-keys-mode keymap.")
(define-minor-mode my-keys-mode
  t " my-keys" 'my-keys-mode-map)
(my-keys-mode 1)
(defun my-minibuffer-setup-hook ()
  (my-keys-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

; Appearance
(load-theme 'Amelie t)
(setq ring-bell-function 'ignore)
(setq scroll-margin 0 scroll-step 1 scroll-conservatively 10000)
(setq inhibit-startup-message t)
(when window-system
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(set-face-attribute 'default nil :font "Monaco-14")
(require 'auto-complete-config)
(ac-config-default)
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-foreground 'show-paren-match-face "red")
(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :background "#111")
(setq evil-flash-delay 36000)
(set-cursor-color "white")
(ansi-color-for-comint-mode-on)
(setq redisplay-dont-pause t)
(load "~/.emacs.d/my-modeline.el")

; Navigation
(require 'ace-jump-mode)

; File
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq vc-follow-symlinks t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(toggle-diredp-find-file-reuse-dir 1)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":")
(setq bookmark-default-file "~/.emacs.d/bookmarks" bookmark-save-flag 1)
(setq delete-by-moving-to-trash t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Buffer
(iswitchb-mode t)
(require 'tabbar)
(setq tabbar-buffer-groups-function
      (lambda ()
        (list (cond
               ((string-equal "*" (substring (buffer-name) 0 1)) "System")
               (t "User")
               ))))
(set-face-attribute 'tabbar-default nil :background "black" :foreground "white" :box nil)
(set-face-attribute 'tabbar-unselected nil :background "black" :foreground "white" :box nil)
(set-face-attribute 'tabbar-selected nil :background "white" :foreground "black" :height 1.2 :box nil)
(set-face-attribute 'tabbar-highlight nil :background "black" :foreground "white" :box nil)
(set-face-attribute 'tabbar-button nil :background "black" :foreground "white" :box nil)
(set-face-attribute 'tabbar-separator nil :background "black" :foreground "white" :box nil)
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " + " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)
(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)
(mouse-wheel-mode 1)
(tabbar-mode 1)
(tabbar-mwheel-mode -1)

; Editing
(setq-default indent-tabs-mode nil)
(setq evil-shift-width 2)
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence 2 300 2))
(add-hook 'python-mode-hook '(lambda ()
                               (setq python-indent 2)))
(electric-pair-mode 1)

; Load evil
(add-to-list 'load-path "~/.emacs.d/evil")
(setq evil-default-cursor t)
(require 'evil)
(evil-mode 1)

; mode line color
(add-hook 'evil-normal-state-entry-hook 'set-mode-line-color)
(add-hook 'evil-insert-state-entry-hook 'set-mode-line-color)
(add-hook 'evil-replace-state-entry-hook 'set-mode-line-color)
(add-hook 'evil-visual-state-entry-hook 'set-mode-line-color)
(add-hook 'evil-operator-state-entry-hook 'set-mode-line-color)
(defun set-mode-line-color ()
  (interactive)
  (cond
   ((evil-normal-state-p)
    (set-face-background 'modeline "#111"))
   ((evil-insert-state-p)
    (set-face-background 'modeline "#09c"))
   ((evil-replace-state-p)
    (set-face-background 'modeline "#0c9"))
   ((evil-visual-state-p)
    (set-face-background 'modeline "#90c"))
   ((evil-operator-state-p)
    (set-face-background 'modeline "#c09"))
    ))

; Remote
(setq tramp-default-method "ssh")
(setq tramp-verbose 10)

; KEYS

; edit
(define-key evil-insert-state-map "\C-\\" 'delete-horizontal-space)

; navigate
(define-key evil-normal-state-map "M" 'scroll-up)
(define-key evil-normal-state-map "U" 'scroll-down)
(define-key evil-normal-state-map "e" 'ace-jump-char-mode)
(define-key my-keys-mode-map "\C-l"
  (lambda () (interactive)
    (progn (tabbar-forward-tab) (set-mode-line-color))))
(define-key my-keys-mode-map "\C-k"
  (lambda () (interactive)
    (progn (tabbar-backward-tab) (set-mode-line-color))))
(define-key my-keys-mode-map "\C-j"
  (lambda () (interactive)
    (progn (tabbar-forward-group) (set-mode-line-color))))
(define-key evil-normal-state-map "s" 'evil-find-char-backward)

; comma commands
(define-key evil-normal-state-map ",1" 'delete-other-windows)
(define-key evil-normal-state-map ",q"
  (lambda () (interactive)
    (progn (kill-this-buffer) (set-mode-line-color))))
(define-key evil-normal-state-map ",Q" 'evil-save-and-quit)
(define-key evil-normal-state-map ",w" 'evil-write)
(define-key evil-normal-state-map ",e" 'eval-last-sexp)
(define-key evil-normal-state-map ",r" 'execute-extended-command)
(define-key evil-normal-state-map ",t" 'undo-tree-redo)
(define-key evil-normal-state-map ",a" 'evil-window-next)
(define-key evil-normal-state-map ",s" 'shell)
(define-key evil-normal-state-map ",f" 'ido-find-file)
(define-key evil-normal-state-map ",g" 'evil-scroll-line-to-top)
(define-key evil-normal-state-map ",z" 'save-buffers-kill-terminal)
(define-key evil-normal-state-map ",c" 'compile)

; mode
(define-key evil-visual-state-map "q" 'evil-force-normal-state)
(define-key evil-insert-state-map "j" 'cofi/maybe-exit)
(define-key evil-replace-state-map "j" 'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit
  ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j) nil 0.5)))
      (cond
        ((null evt) (message ""))
        ((and (integerp evt) (char-equal evt ?j))
         (delete-char -1)
         (set-buffer-modified-p modified)
         (push 'escape unread-command-events))
        (t (setq unread-command-events (append unread-command-events (list evt))))))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("5d6042a3b78fcd82d6bbcaca5cfd26fa3ef3a47eb5d402948e628f265662d717" "84b941babe3bb3bc58b04a6a336992ef4751401758d5aff4aa3fade27194d5cc" "cc83fa4ffec1545d4bde6a44b1fb8431f9090874a22554920c709fa97338d0aa" "a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" "3580fb8e37ee9e0bcb60762b81260290329a97f3ca19249569d404fce422342f" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
