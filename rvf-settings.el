;; Input method for multilingual text, activated by C-\
(setq default-input-method 'spanish-prefix)

;; http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/
(setq confirm-nonexistent-file-or-buffer nil)

;; from https://github.com/magnars/.emacs.d/blob/master/init.el
;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; dragging the mouse over a stretch of text also adds the text to the
;; kill ring.
(setq mouse-drag-copy-region t)

(require 'ido)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

(show-paren-mode 1)
