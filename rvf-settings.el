;; Input method for multilingual text, activated by C-\
(setq default-input-method 'spanish-prefix)

;; http://www.emacswiki.org/emacs-en/InputMethods
(setq input-activate nil)
(add-hook 'find-file-hook
	  (lambda ()(if (eq input-activate t) (toggle-input-method))))

;; http://www.masteringemacs.org/articles/2010/11/14/disabling-prompts-emacs/
(setq confirm-nonexistent-file-or-buffer nil)

;; dragging the mouse over a stretch of text also adds the text to the
;; kill ring.
(setq mouse-drag-copy-region t)

(require 'ido)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; fix scroll
;; from http://zeekat.nl/articles/making-emacs-work-for-me.html
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; see http://stackoverflow.com/questions/8524490/emacs-as-finder-explorer-copy-files
(setq dired-dwim-target t)

(setq magit-turn-on-auto-revert-mode t)
(setq magit-set-upstream-on-push t)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; org-ref

(setq org-ref-default-bibliography '("~/Dropbox/texmf/bibtex/bib/misc/rvf.bib"))
(setq org-ref-pdf-directory "~/Dropbox/References/")
