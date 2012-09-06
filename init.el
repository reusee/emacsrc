; Package
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

; System
(require 'cl)
(when window-system
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(add-hook 'server-switch-hook (lambda () ; client模式下，关闭buffer时不提示
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))
(add-to-list 'auto-mode-alist (cons (file-truename "~/.emacs") 'emacs-lisp-mode)) ; .emacs是symlink的话也自动进入emacs-lisp-mode
(helm-mode 1)
(icy-mode 1)

; Compile
(setq compile-command "")
(define-key evil-insert-state-map "\C-o" 'save-and-recompile)
(evil-define-command save-and-recompile
  ()
  (interactive)
  (progn (save-buffer) (recompile) (other-window)))

; Shell
(setenv "SHELL" "bash")
(defadvice shell (around always-new-shell activate) ; 重命名新打开的buffer
  (let ((buffer (generate-new-buffer-name "*shell*"))) ad-do-it))
(defun auto-kill-buffer-when-exit (process event) ; shell中exit命令后自动关闭buffer
  (when (memq (process-status process) '(signal exit))
      (kill-buffer (process-buffer process))))
(add-hook 'shell-mode-hook
          #'(lambda () (set-process-sentinel (get-buffer-process (current-buffer))
                                             'auto-kill-buffer-when-exit)))
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg) activate) ; ansi-term里exit命令会关闭buffer
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(defvar my-term-shell "/bin/bash") ; 执行ansi-term时不询问采用的shell
(defadvice ansi-term (before force-bash activate)
  (interactive (list my-term-shell)))
(defun my-term-hook () ; 让ansi-term里的url可以点击
  (goto-address-mode))
(add-hook 'term-mode-hook 'my-term-hook)

; Override Minor Mode
(defvar my-keys-mode-map (make-keymap) "my-keys-mode keymap.")
(define-minor-mode my-keys-mode
  t " my-keys" 'my-keys-mode-map)
(my-keys-mode 1)
(defun my-minibuffer-setup-hook () ; minibuffer里不采用此minor mode
  (my-keys-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

; Appearance
(load-theme 'Amelie t)
(setq ring-bell-function 'ignore) ; 禁用蜂鸣器
(setq scroll-margin 0 scroll-step 1 scroll-conservatively 10000) ; 平滑滚动
(setq inhibit-startup-message t) ; 不显示启动页面
(when window-system
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ; 鼠标滚动行数
  (setq mouse-wheel-progressive-speed nil) ; 鼠标没有加速
  (tool-bar-mode -1) ; 禁用工具栏
  (scroll-bar-mode -1) ; 禁用滚动条
)
(menu-bar-mode -1) ; 禁用菜单栏
(blink-cursor-mode -1) ; 禁止光标闪烁
(set-face-attribute 'default nil :font "Monaco-14") ; 默认字体
(show-paren-mode 1) ; 高亮显示匹配的括号
(setq show-paren-delay 0)
(set-face-foreground 'show-paren-match-face "red")
(global-hl-line-mode 1) ; 高亮当前行
(set-face-attribute hl-line-face nil :background "#111")
(setq evil-flash-delay 36000) ; 搜索结果高亮时间
(set-cursor-color "white") ; 光标颜色
(ansi-color-for-comint-mode-on) ; shell和term下显示ansi标准的颜色
(setq redisplay-dont-pause t) ; 据说可以加速渲染
(load "~/.emacs.d/my-modeline.el") ; modeline的定义
(setq initial-scratch-message "")

; auto complete
(require 'auto-complete-config)
(ac-config-default)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers
                           ac-source-abbrev
                           ac-source-dictionary))
(setq ac-ignore-case nil) ; 自动完成时，大小写敏感

; Navigation
(require 'ace-jump-mode)

; File
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq vc-follow-symlinks t) ; 打开被版本控制系统控制的文件时，不询问
(toggle-diredp-find-file-reuse-dir 1) ; 用dired打开文件时，使用当前buffer，而不是新建一个
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward uniquify-separator ":") ; 打开同名文件时，加上目录名作为buffer名
(setq bookmark-default-file "~/.emacs.d/bookmarks" bookmark-save-flag 1)
(setq delete-by-moving-to-trash t) ; 使用系统垃圾箱删除文件
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; 保存之前删除行尾的空格

; Magic Mode
(add-to-list 'magic-mode-alist '("#!/usr/bin/env python" . python-mode))

; Backup
(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))
(setq version-control t) ; 开启备份文件版本控制
(setq kept-old-versions 2) ; 保存最初的文件
(setq kept-new-versions 256) ; 保存最新的文件
(setq delete-old-versions t) ; 删除不符合以上条件的文件
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; Buffer
(iswitchb-mode t)
(require 'tabbar)
(setq tabbar-buffer-groups-function ; tabbar中的buffer分组
      (lambda ()
        (list (cond
               ((string-equal "*" (substring (buffer-name) 0 1)) "System")
               (t "User")
               ))))
(setq tabbar-buffer-list-function ; tabbar中的buffer过滤
      (lambda ()
        (delq nil
              (mapcar #'(lambda (b)
                          (cond
                           ((string-match "^\\*helm-mode-" (buffer-name b)) nil)
                           ((member (buffer-name b) '(
                                                    "*helm mini*"
                                                    "*Helm Find Files*"
                                                    "*Help*"
                                                    "*Packages*"
                                                    "*Completions*"
                                                    "*Compile-Log*"
                                                    )) nil)
                           ((eq (current-buffer) b) b)
                           ((buffer-file-name b) b)
                           ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                           ((buffer-live-p b) b)
                           ))
                      (buffer-list)))))
(set-face-attribute 'tabbar-default nil :background "black" :foreground "white" :box nil)
(set-face-attribute 'tabbar-unselected nil :background "black" :foreground "gray70" :box nil)
(set-face-attribute 'tabbar-selected nil :background "black" :foreground "white" :box nil :weight 'bold)
(set-face-attribute 'tabbar-highlight nil :background "black" :foreground "white" :box nil)
(set-face-attribute 'tabbar-button nil :background "black" :foreground "gray70" :box nil)
(set-face-attribute 'tabbar-separator nil :background "black" :foreground "white" :box nil)
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate) ; 修改过的文件，tabbar上显示+号
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
(mouse-wheel-mode 1) ; 启用鼠标滚轮
(tabbar-mode 1)
(tabbar-mwheel-mode -1) ; 禁用鼠标滚轮切换tab
(define-key my-keys-mode-map "\C-l" 'tabbar-forward-tab) ; 全局的tab控制键
(define-key my-keys-mode-map "\C-k" 'tabbar-backward-tab)
(define-key my-keys-mode-map "\C-j" 'tabbar-forward-group)

; Editing
(setq-default indent-tabs-mode nil) ; 禁用tab缩进
(setq evil-shift-width 2) ; 缩进量
(setq-default tab-width 2)
(setq-default tab-stop-list (number-sequence 2 300 2))
(add-hook 'python-mode-hook '(lambda () ; 设python模式下的缩进量
                               (setq python-indent 2)))
(electric-pair-mode 1) ; 自动输入成对的括号

; Load evil
(setq evil-default-cursor t) ; 使用主题默认的光标
(require 'evil)
(evil-mode 1)

; mode line color
(add-hook 'evil-normal-state-entry-hook 'set-mode-line-color) ; 进入另一个模式时，自动切换modeline的颜色
(add-hook 'evil-insert-state-entry-hook 'set-mode-line-color)
(add-hook 'evil-replace-state-entry-hook 'set-mode-line-color)
(add-hook 'evil-visual-state-entry-hook 'set-mode-line-color)
(add-hook 'evil-operator-state-entry-hook 'set-mode-line-color)
(add-hook 'window-configuration-change-hook 'set-mode-line-color) ; 窗口有变化时自动切换modeline颜色
(defun set-mode-line-color ()
  (interactive)
  (cond
   ((string-equal mode-name "Help")
    (set-face-background 'modeline "red"))
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
   ((evil-emacs-state-p)
    (set-face-background 'modeline "#c90"))
   ((evil-motion-state-p)
    (set-face-background 'modeline "#9c0"))
    ))

; KEYS

; edit
(define-key evil-insert-state-map "\C-\\" 'delete-horizontal-space)

; normal state command
(define-key evil-normal-state-map "q" 'evil-visual-block)
(define-key evil-normal-state-map "e" 'ace-jump-char-mode)
(define-key evil-normal-state-map "U" 'scroll-down)
(define-key evil-normal-state-map "s" 'evil-find-char-backward)
(define-key evil-normal-state-map "H" 'tabbar-backward-tab)
(define-key evil-normal-state-map "L" 'tabbar-forward-tab)
(define-key evil-normal-state-map "M" 'scroll-up)

; comma commands
(define-key evil-normal-state-map ",1" 'delete-other-windows)
(define-key evil-normal-state-map ",2" 'split-window-below)
(define-key evil-normal-state-map ",q" 'kill-this-buffer)
(define-key evil-normal-state-map ",Q" 'evil-save-and-quit)
(define-key evil-normal-state-map ",w" 'evil-write)
(define-key evil-normal-state-map ",e" 'eval-last-sexp)
(define-key evil-visual-state-map ",e" 'eval-region)
(define-key evil-normal-state-map ",r" 'execute-extended-command)
(define-key evil-normal-state-map ",t" 'undo-tree-redo)
(define-key evil-normal-state-map ",a" 'evil-window-next)
(define-key evil-normal-state-map ",s" 'shell)
(define-key evil-normal-state-map ",S" 'ansi-term)
(define-key evil-normal-state-map ",f" 'find-file)
(define-key evil-normal-state-map ",gt" 'evil-scroll-line-to-top)
(define-key evil-normal-state-map ",gg" 'evil-scroll-line-to-center)
(define-key evil-normal-state-map ",gb" 'evil-scroll-line-to-bottom)
(define-key evil-normal-state-map ",z" 'save-buffers-kill-terminal)
(define-key evil-normal-state-map ",x" 'icicle-pp-eval-expression)
(define-key evil-normal-state-map ",c" 'compile)
(define-key evil-normal-state-map ",v" 'iswitchb-buffer)
(define-key evil-normal-state-map ",ba" 'bookmark-set)
(define-key evil-normal-state-map ",bd" 'bookmark-delete)
(define-key evil-normal-state-map ",bj" 'bookmark-jump)
(define-key evil-normal-state-map ",m" 'evil-record-macro)
(define-key evil-normal-state-map ",n" 'evil-execute-macro)

; mode
(define-key evil-visual-state-map "q" 'evil-force-normal-state)
(define-key evil-insert-state-map "k" 'cofi/kd) ; 用kd返回normal状态
(define-key evil-replace-state-map "k" 'cofi/kd)
(define-key evil-insert-state-map "j" 'cofi/jj) ; 用jj返回normal状态
(define-key evil-replace-state-map "j" 'cofi/jj)
(evil-define-command cofi/kd () (interactive) (cofi/maybe-normal "k" ?d))
(evil-define-command cofi/jj () (interactive) (cofi/maybe-normal "j" ?j))
(evil-define-command cofi/maybe-normal
  (first second)
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert first)
    (let ((evt (read-event nil nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt second))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events (list evt))))))))

(custom-set-variables
 '(custom-safe-themes (quote ("5d6042a3b78fcd82d6bbcaca5cfd26fa3ef3a47eb5d402948e628f265662d717" "84b941babe3bb3bc58b04a6a336992ef4751401758d5aff4aa3fade27194d5cc" "cc83fa4ffec1545d4bde6a44b1fb8431f9090874a22554920c709fa97338d0aa" "a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" "3580fb8e37ee9e0bcb60762b81260290329a97f3ca19249569d404fce422342f" default))))
(custom-set-faces
 )
