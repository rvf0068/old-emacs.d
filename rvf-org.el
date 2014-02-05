(require 'org-protocol)
(require 'org-beamer)

(require 'ob-latex)

(require 'ox-latex)
(require 'ox-beamer)
(require 'ox-md)

(add-to-list 'org-latex-classes
             '("beamer" "\\documentclass[presentation]{beamer}
\[DEFAULT-PACKAGES]
\[PACKAGES]
\[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-packages-alist '("" "listings"))

;; from http://lists.gnu.org/archive/html/emacs-orgmode/2013-10/msg00322.html
(add-to-list 'org-beamer-environments-extra
             '("onlyenv" "O" "\\begin{onlyenv}%a" "\\end{onlyenv}"))

(add-hook 'org-mode-hook 'turn-on-auto-revert-mode)

(add-hook 'org-mode-hook 'turn-on-org-cdlatex)

(add-hook 'org-mode-hook 'smartparens-mode)

(add-hook 'org-mode-hook 'abbrev-mode)

;; from the info documentation
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (yas/minor-mode-on)
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

;; useful for math in org-mode
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "$")
			   (lambda () (interactive)
			     (insert "\\(\\)")
			     (forward-char -2)))
	    ))


(setq org-export-with-tags nil)
(setq org-hide-emphasis-markers t)
(setq org-latex-listings t)
(setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))
(setq org-log-done 'note)
(setq org-return-follows-link t)
(setq org-src-fontify-natively t)
(setq org-support-shift-select 'always)

; the space at the beginning is useful to move the cursor
(setq org-agenda-prefix-format 
      '((agenda . " %i %-12:c%?-12t% s")
	(timeline . "  % s")
	(todo . " %-12:c")
	(tags . " %-12:c")
	(search . " %-12:c")))

(setq org-agenda-entry-text-leaders "    ")

(setq org-file-apps
      '((auto-mode . emacs)
	("pdf" . "evince %s")
	("djvu" . "evince %s")
	("epub" . "fbreader %s")
	("html" . "firefox %s")
	))


