(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'evil-leader) (global-evil-leader-mode) (evil-leader/set-leader ",")
(require 'evil) (evil-mode 1)
(require 'key-chord) (key-chord-mode 1)
(require 'ace-jump-mode)
(require 'helm-config)
(require 'auto-complete-config) (ac-config-default) (semantic-mode t)
(require 'elscreen) (elscreen-start)
(require 'linum-relative) (global-linum-mode 1)

(setq explicit-shell-file-name "/usr/bin/bash")

(setq backup-directory-alist `((".*" . "~/.emacs.d/saves")))
(setq auto-save-file-name-transforms `((".*", "~/.emacs.d/saves", t)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default default-tab-width 2)
(setq-default evil-shift-width 2)
(setq-default tab-stop-list (number-sequence 2 300 2))

(kill-buffer "*scratch*")
(load-theme 'molokai t)
(menu-bar-mode -1)
(show-paren-mode 1) (setq show-paren-delay 0)
(setq evil-flash-delay 36000)
(setq redisplay-dont-pause t)
(lexical-let ((default-color '("#333333" . "#ffffff")))
  (add-hook 'post-command-hook
	    (lambda ()
	      (let ((color (cond ((minibufferp) default-color)
				 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
				 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
				 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
				 (t default-color))))
		(set-face-background 'mode-line (car color))
		(set-face-foreground 'mode-line (cdr color))))))
(setq elscreen-tab-display-control nil)
(setq elscreen-tab-display-kill-screen nil)
(set-face-attribute 'elscreen-tab-background-face nil :background "black" :foreground "whie" :underline nil)
(set-face-attribute 'elscreen-tab-current-screen-face nil :background "black" :foreground "lightgreen" :underline nil)
(set-face-attribute 'elscreen-tab-other-screen-face nil :background "black" :foreground "grey" :underline nil)
(set-face-attribute 'linum nil :background "black" :foreground "#337733")

(define-key evil-motion-state-map (kbd "RET") nil)
(define-key evil-motion-state-map " " nil)

(define-key evil-normal-state-map " " 'evil-ex)
(define-key evil-normal-state-map "M" 'evil-scroll-down)
(define-key evil-normal-state-map "U" 'evil-scroll-up)
(define-key evil-normal-state-map "s" 'evil-find-char-backward)
(define-key evil-normal-state-map "H" 'elscreen-previous)
(define-key evil-normal-state-map "L" 'elscreen-next)

(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)
(key-chord-define evil-insert-state-map "kd" 'evil-normal-state)
(key-chord-define evil-replace-state-map "kd" 'evil-normal-state)

(defun make-ex (cmd) 
  (lexical-let ((cmd cmd)) (lambda () (interactive) (evil-ex cmd))))
(evil-leader/set-key
  "1" 'delete-other-windows
  "2" 'split-window-below

  "q" 'elscreen-kill
  "w" 'evil-write
  "e" 'eval-last-sexp
  "r" 'execute-extended-command
  "t" (lambda () (interactive) (elscreen-create) (evil-ex "e "))

  "a" (make-ex "!ack ")
  "s" 'shell
  "d" 'helm-mini
  "f" 'ace-jump-char-mode

  "z" 'evil-ex-nohighlight
  "x" 'save-buffers-kill-terminal
  "c" (make-ex "!")
  "v" 'evil-visual-block
  "b" 'switch-to-buffer
  )
