(setq-default
 mode-line-format
 '(
   (:propertize (:eval (format-time-string "%H:%M")))
   (:propertize "%4l" face mode-line-position-face)
   " "
   (:eval
    (cond (buffer-read-only
           (propertize "RO" 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize "**" 'face 'mode-line-modified-face))
          (t "  ")))
   " "
   "%["
   (:propertize mode-name
                face mode-line-mode-face)
   "%]"
   (:propertize evil-mode-line-tag
                face mode-line-evil-state-face)
   (:propertize (:eval (shorten-directory default-directory 50))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ))

(defun shorten-directory (dir max-length)
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "../" output)))
    output))

(make-face 'mode-line-position-face)
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-evil-state-face)

(set-face-attribute 'mode-line nil
                    :foreground "white" :background "#111")
(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#4271ae"
                    :box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829" :background "#ffffff"
                    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-mode-face nil
                    :foreground "yellow")
(set-face-attribute 'mode-line-filename-face nil
                    :foreground "green" :weight 'bold)
(set-face-attribute 'mode-line-evil-state-face nil
                    :foreground "pink" :weight 'bold)
