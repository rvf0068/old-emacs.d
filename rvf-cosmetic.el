(setq frame-title-format "%b: %+%+ %f")

;; to have the column number displayed
(column-number-mode t)

;; blink instead of beep when an error occurs
(setq visible-bell t)

(load-theme 'zenburn t)
(set-face-attribute 'mode-line nil :height 0.8)

;; new to newer Emacs
(set-face-attribute 'mode-line-inactive nil :height 0.8)

(display-time)
