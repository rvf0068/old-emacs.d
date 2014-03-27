(require 'org-protocol)
(require 'org-beamer)

(require 'ob-latex)
(require 'ob-org)
(require 'ob-R)

(require 'ox-beamer)
(require 'ox-bibtex)
(require 'ox-latex)
(require 'ox-md)

(add-to-list 'org-latex-packages-alist '("" "listings"))

;; from http://lists.gnu.org/archive/html/emacs-orgmode/2013-10/msg00322.html
(add-to-list 'org-beamer-environments-extra
             '("onlyenv"   "O" "\\begin{onlyenv}%a"     "\\end{onlyenv}"))

(add-to-list 'org-beamer-environments-extra
	     '("corollary" "r" "\\begin{corollary}%a%U" "\\end{corollary}"))
(add-to-list 'org-beamer-environments-extra
	     '("lemma"     "l" "\\begin{lemma}%a%U"     "\\end{lemma}"))

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


(defun org-cdlatex-complex-numbers ()
  (interactive)
  (if (org-inside-LaTeX-fragment-p)
      (insert "\\mathbb{C}")
      (insert "C")
     ))

(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C")
		'org-cdlatex-complex-numbers
	    )))


(defun org-cdlatex-real-numbers ()
  (interactive)
  (if (org-inside-LaTeX-fragment-p)
      (insert "\\mathbb{R}")
      (insert "R")
     ))

(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "R")
		'org-cdlatex-real-numbers
	    )))

(defun org-cdlatex-rational-numbers ()
  (interactive)
  (if (org-inside-LaTeX-fragment-p)
      (insert "\\mathbb{Q}")
      (insert "Q")
     ))

(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "Q")
		'org-cdlatex-rational-numbers
	    )))


;; see https://lists.nongnu.org/archive/html/emacs-orgmode/2014-02/msg00223.html
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-export-copy-to-kill-ring nil)
(setq org-export-with-tags nil)
(setq org-hide-emphasis-markers t)
(setq org-latex-listings t)
(setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))
(setq org-log-done 'note)
(setq org-publish-timestamp-directory "~/Dropbox/org/org-timestamps/")
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

(setq org-latex-listings-options
      '(("basicstyle" "\\ttfamily")
	("commentstyle" "\\itshape\\ttfamily")
	("keywordstyle" "\\bfseries")
	("showspaces" "false")
	("showstringspaces" "false")
	("breaklines" "true")
	("breakautoindent" "true")))

;; remove strike-through emphasis, see
;; http://stackoverflow.com/a/22498697/577007
(setq org-emphasis-alist
      '(("*" bold)
	("/" italic)
	("_" underline)
	("=" org-verbatim verbatim)
	("~" org-code verbatim)))

;; this overwrites the definition in org.el
(defun org-reftex-citation ()
  (interactive)
  (let ((reftex-docstruct-symbol 'rds)
	;; the next line was modified
	(reftex-cite-format "[[cite:%l]]")
	rds bib)
    (save-excursion
      (save-restriction
	(widen)
	(let ((case-fold-search t)
	      (re "^#\\+bibliography:[ \t]+\\([^ \t\n]+\\)"))
	  (if (not (save-excursion
		     (or (re-search-forward re nil t)
			 (re-search-backward re nil t))))
	      (error "No bibliography defined in file")
	    (setq bib (concat (match-string 1) ".bib")
		  rds (list (list 'bib bib)))))))
    (call-interactively 'reftex-citation)))

;; for exporting images according to backend
;; see https://lists.gnu.org/archive/html/emacs-orgmode/2014-02/msg00296.html
(defmacro by-backend (&rest body)
  `(case (if (boundp 'backend) (org-export-backend-name backend) nil) ,@body))

;; filters for octopress
(defun my-math-replacement (contents backend info)
  (when (eq backend 'md)
    (replace-regexp-in-string "\\\\(\\|\\\\)\\|\\\\\\[\\|\\\\\\]" "$$" contents)
    ))

(add-to-list 'org-export-filter-latex-fragment-functions
	     'my-math-replacement)

(defun my-space-replacement (contents backend info)
  (when (eq backend 'md)
    (replace-regexp-in-string "\n\s *" " " contents)
    ))

(add-to-list 'org-export-filter-latex-fragment-functions
	     'my-space-replacement)

;; modified from https://github.com/spacemanaki/octorgopress
(defun org-octopress-template (contents backend info)
  (when (eq backend 'md)
  (let ((title (car (plist-get info :title)))
        (date (car (plist-get info :date)))
        (time "")
        (keywords (plist-get info :keywords))
        (frontmatter
         "---
layout: post
title: %s
date: %s %s
comments: true
categories: %s
---
"))
    (if keywords
        (concat (format frontmatter title date time keywords) contents)
        (concat (format frontmatter title date time "") contents)
      )
    )))


(add-to-list 'org-export-filter-final-output-functions
	     'org-octopress-template)
