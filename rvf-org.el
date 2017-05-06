(require 'org-protocol)
(require 'org-mime)

(require 'ob-latex)
(require 'ob-org)
(require 'ob-R)
(require 'ob-ipython)

(require 'ox-beamer)
(require 'ox-bibtex)
(require 'ox-latex)
(require 'ox-md)

(setq org-latex-default-packages-alist
      (remove '("" "hyperref" nil)
	      org-latex-default-packages-alist))

(setq org-latex-default-packages-alist
      (nconc org-latex-default-packages-alist
	     '(("colorlinks=true, linkcolor=blue" "hyperref" t)
	       )))

(add-to-list 'org-latex-classes
	     '("libertine"
	       "\\documentclass{article}
               \\usepackage[AUTO]{inputenc}
               \\usepackage[AUTO]{babel}
               \\usepackage[mono=false]{libertine}
               \\usepackage[libertine,timesmathacc]{newtxmath}
               \\usepackage[scaled=0.7]{luximono}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ))

(add-to-list 'org-latex-classes
	     '("two-column"
	       "\\documentclass[twocolumn]{article}
               \\usepackage[AUTO]{inputenc}
               \\usepackage[AUTO]{babel}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
	       '("beamer-talk"
		 "\\documentclass{beamer}
\\usepackage[AUTO]{inputenc}
\\usepackage{tikz}
\\usepackage{arev}
\\usepackage{enumitem}
\\setitemize{label=\\usebeamerfont*{itemize item}%
  \\usebeamercolor\[fg]{itemize item}
  \\usebeamertemplate{itemize item}}
\\setlist{leftmargin=*,labelindent=0cm}
\\setenumerate\[1]{%
  label=\\protect\\usebeamerfont{enumerate item}%
        \\protect\\usebeamercolor\[fg]{enumerate item}%
        \\insertenumlabel.}
\\setbeamertemplate{navigation symbols}{}
\\setbeamertemplate{items}[circle]
\\usefonttheme{professionalfonts}
\[NO-DEFAULT-PACKAGES]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

;; from http://lists.gnu.org/archive/html/emacs-orgmode/2017-03/msg00190.html
(add-to-list 'org-latex-classes
	     '("exam" "\\documentclass[12pt, addpoints, answers]{exam}"
	       ("\\begin{questions}%%%s" "\\end{questions}" "\\begin{questions}%%%s" "\\end{questions}")
	       ("\\begin{question}%%%s" "\\end{question}" "\\begin{question*}%%%s" "\\end{question*}")
	       ("\\begin{parts}%%%s" "\\end{parts}" "\\begin{parts}%%%s" "\\end{parts}")
	       ("\\begin{part}[%s]" "\\end{part}" "\\begin{part*}[%s]" "\\end{part*}")
	       ("\\begin{solution}[%s]" "\\end{solution}" "\\begin{solution}[%s]" "\\end{solution}")))

;; from http://lists.gnu.org/archive/html/emacs-orgmode/2013-10/msg00322.html
(setq org-beamer-environments-extra
      (nconc org-beamer-environments-extra
	     '(("onlyenv"   "O" "\\begin{onlyenv}%a"     "\\end{onlyenv}")
	       ("corollary" "r" "\\begin{corollary}%a%U" "\\end{corollary}")
	       ("lemma"     "l" "\\begin{lemma}%a%U"     "\\end{lemma}")
	       ("proposition"     "P" "\\begin{proposition}%a%U"     "\\end{proposition}")
	       )))

;; GAP suport is included in the listings package
(add-to-list 'org-src-lang-modes '("GAP" . gap))

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;; from the info documentation
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

;; idea from 
;; http://endlessparentheses.com/keymap-for-launching-external-applications-and-websites.html
(defmacro field-org (exec symb)
  "Return a function to print a mathbb letter."
  (let ((func-name (intern (concat "org-cdlatex-" exec))))
    `(progn
       (defun ,func-name ()
         ,(format "Print the %s numbers." exec)
         (interactive)
	 (if (org-inside-LaTeX-fragment-p)
	     (if (looking-back (concat "\\\\mathbb{" ,symb "}"))
		 (progn (delete-char -10)
			(insert ,symb))
	       (insert "\\mathbb{" ,symb "}"))
	   (insert ,symb)
	   ))
       (define-key org-mode-map ,symb ',func-name)
       #',func-name)))

;; inside math mode, pressing C once gives \mathbb{C}. Twice gives C.
(field-org "complex" "C")
(field-org "real" "R")
;; (field-org "rational" "Q")

(defmacro mathcal-org (symb)
  "Return a function to print a mathcal letter."
  (let ((func-name (intern (concat "org-cal-cdlatex-" symb))))
    `(progn
       (defun ,func-name ()
         ,(format "Print a letter %s inside mathcal." symb)
         (interactive)
	 (if (org-inside-LaTeX-fragment-p)
	     (if (looking-back (concat "\\\\mathcal{" ,symb "}"))
		 (progn (delete-char -11)
			(insert ,symb))
	       (insert "\\mathcal{" ,symb "}"))
	   (insert ,symb)
	   ))
       (define-key org-mode-map ,symb ',func-name)
       #',func-name)))

;;(mathcal-org "A")

;; from Nicolas Richard <theonewiththeevillook@yahoo.fr>
;; Date: Fri, 8 Mar 2013 16:23:02 +0100
;; Message-ID: <87vc913oh5.fsf@yahoo.fr>
(defun yf/org-electric-dollar nil
  "When called once, insert \\(\\) and leave point in between.
When called twice, replace the previously inserted \\(\\) by one $."
  (interactive)
  (if (and (looking-at "\\\\)") (looking-back "\\\\("))
      (progn (delete-char 2)
             (delete-char -2)
             (insert "$"))
    (insert "\\(\\)")
    (backward-char 2)))

(defun org-absolute-value ()
  "Insert || and leave point inside when pressing |"
  (interactive)
  (if (org-inside-LaTeX-fragment-p)
      (progn
	(insert "||")
	(backward-char 1)
	)
    (insert "|")
    )
  )

;; see http://stackoverflow.com/a/25778692/577007
(add-hook 'org-mode-hook
	  (lambda ()
	    (turn-on-auto-revert-mode)
	    (turn-on-org-cdlatex)
	    (smartparens-mode 1)
	    (abbrev-mode 1)
	    ;; yasnippets
            (yas/minor-mode-on)
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas-next-field)
	    (local-set-key (kbd "$") 'yf/org-electric-dollar)
	    (local-set-key (kbd "|") 'org-absolute-value)
	    ))

;; see https://lists.nongnu.org/archive/html/emacs-orgmode/2014-02/msg00223.html
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; see http://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(setq org-ellipsis " ⤵")

(setq org-export-copy-to-kill-ring nil)
(setq org-export-with-tags nil)
(setq org-latex-listings t)
;; (setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))
(setq org-log-done 'note)
(setq org-publish-timestamp-directory "~/Dropbox/org/org-timestamps/")
(setq org-return-follows-link t)
(setq org-src-fontify-natively t)
(setq org-src-preserve-indentation t)
(setq org-support-shift-select 'always)

;; org-pomodoro
(setq org-pomodoro-keep-killed-pomodoro-time t)

;; see http://stackoverflow.com/questions/28126222/latex-math-mode-font-color-in-org-mode
(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex)))

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
	("commentstyle" "\\itshape\\ttfamily\\color{green!50!black}")
	("keywordstyle" "\\bfseries\\color{blue}")
	("stringstyle" "\\color{purple}")
	("breaklines" "true")
	("showstringspaces" "false")
	))

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
;; (defmacro by-backend (&rest body)
;;   `(case (if (boundp 'backend) (org-export-backend-name backend) nil) ,@body))
;; see https://lists.gnu.org/archive/html/emacs-orgmode/2015-09/msg00118.html
(defmacro by-backend (&rest body)
  `(case org-export-current-backend ,@body))



;; (defun my-space-replacement (contents backend info)
;;   (when (eq backend 'md)
;;     (replace-regexp-in-string "\n\s *" " " contents)
;;     ))

;; (add-to-list 'org-export-filter-latex-fragment-functions
;; 	     'my-space-replacement)

;; filters for beamer export
(defun my-beamer-replacement (contents backend info)
  (when (eq backend 'beamer)
    (replace-regexp-in-string "\\\\usepackage\\[margin=2.5cm\\]{geometry}\n\\|\\\\usepackage\\[colorlinks=true, linkcolor=blue\\]{hyperref}\n\\|\\\\usepackage\\[libertine,timesmathacc\\]{newtxmath}\n\\|\\[:B_corollary:\\]\\|\\[:B_proposition:\\]\\|\\[:B_lemma:\\]\\|\\[:B_definition:\\]\\|\\[:B_example:\\]\\|\\[:B_theorem:\\]\\|:B\\\\(_{\\\\text{exampleblock}}\\\\):\\|:B\\\\(_{\\\\text{theorem}}\\\\):\\|:B\\\\(_{\\\\text{block}}\\\\):BMCOL:" "" contents)
    ))

(add-to-list 'org-export-filter-final-output-functions
	     'my-beamer-replacement)

;; see http://ergoemacs.org/emacs/elisp_idioms.html
(defun my-gaps-code (contents backend info)
  (when (and (eq backend 'beamer) (string-match "semiverbatim" contents))
    (setq contents (replace-regexp-in-string "^[^\\ ]" (lambda (x) (concat "\\\\pause{}" x)) contents))
    (replace-regexp-in-string "gap>"
			      (lambda (x) (concat "{\\\\color{magenta}\\\\textbf{" x "}}")) contents)
    ))

(add-to-list 'org-export-filter-src-block-functions
	     'my-gaps-code)


;; from http://mbork.pl/2013-09-23_Automatic_insertion_of_habit_templates_%28en%29
(defun org-insert-habit ()
  "Insert a new TODO subheading and set its properties so that it becomes a habit."
  (interactive)
  (beginning-of-line)
  (org-insert-todo-subheading nil)
  (org-schedule nil (format-time-string "%Y-%m-%d" (current-time)))
  (save-excursion
    (search-forward ">")
    (backward-char)
    (insert (concat
	     " .+"
	     (read-string "Minimum interval: ")
	     "/"
	     (read-string "Maximum interval: "))))
  (org-set-property "STYLE" "habit")
  (org-set-property "LOGGING" "TODO DONE(!)"))

;; see http://orgmode.org/w/?p=worg.git;a=commitdiff;h=022e5ecafdda4c2a89322251795a437a0918d4c5
(add-to-list 'ispell-skip-region-alist '("#\\+begin_src". "#\\+end_src"))

;; see http://endlessparentheses.com/ispell-and-org-mode.html
(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
;  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  )
(add-hook 'org-mode-hook #'endless/org-ispell)

;; see http://stackoverflow.com/a/19818134/577007
(put 'org-beamer-verbatim-elements 'safe-local-variable (lambda (xx) t))

;; configuration to use org-mime to send subtrees
;; see https://lists.gnu.org/archive/html/emacs-orgmode/2016-10/msg00477.html
;; or http://kitchingroup.cheme.cmu.edu/blog/2016/10/29/Sending-html-emails-from-org-mode-with-org-mime/
;;
;; One sets the property :MAIL_FMT: to html, and the property
;; :MAIL_TO: to the address to send. Then org-mime-subtree prepares
;; the mail buffer.

;; (defun org-mime-subtree ()
;;   "Create an email buffer containing the current org-mode subtree
;;   exported to a org format or to the format specified by the
;;   MAIL_FMT property of the subtree."
;;   (interactive)
;;   (org-mime-send-subtree
;;    (or (org-entry-get nil "MAIL_FMT" org-mime-use-property-inheritance) 'org))
;;   (message-goto-to))

;; (defun org-mime-compose (body fmt file &optional to subject headers)
;;   (require 'message)
;;   (let ((bhook
;; 	 (lambda (body fmt)
;; 	   (let ((hook (intern (concat "org-mime-pre-"
;; 				       (symbol-name fmt)
;; 				       "-hook"))))
;; 	     (if (> (eval `(length ,hook)) 0)
;; 		 (with-temp-buffer
;; 		   (insert body)
;; 		   (goto-char (point-min))
;; 		   (eval `(run-hooks ',hook))
;; 		   (buffer-string))
;; 	       body))))
;; 	(fmt (if (symbolp fmt) fmt (intern fmt)))
;; 	(files (org-element-map (org-element-parse-buffer) 'link
;; 		 (lambda (link)
;; 		   (when (string= (org-element-property :type link) "file")
;; 		     (file-truename (org-element-property :path link)))))))
;;     (compose-mail to subject headers nil)
;;     (message-goto-body)
;;     (cond
;;      ((eq fmt 'org)
;;       (require 'ox-org)
;;       (insert (org-export-string-as
;; 	       (org-babel-trim (funcall bhook body 'org)) 'org t)))
;;      ((eq fmt 'ascii)
;;       (require 'ox-ascii)
;;       (insert (org-export-string-as
;; 	       (concat "#+Title:\n" (funcall bhook body 'ascii)) 'ascii t)))
;;      ((or (eq fmt 'html) (eq fmt 'html-ascii))
;;       (require 'ox-ascii)
;;       (require 'ox-org)
;;       (let* ((org-link-file-path-type 'absolute)
;; 	     ;; we probably don't want to export a huge style file
;; 	     (org-export-htmlize-output-type 'inline-css)
;; 	     (org-html-with-latex 'dvipng)
;; 	     (html-and-images
;; 	      (org-mime-replace-images
;; 	       (org-export-string-as (funcall bhook body 'html) 'html t)))
;; 	     (images (cdr html-and-images))
;; 	     (html (org-mime-apply-html-hook (car html-and-images))))
;; 	(insert (org-mime-multipart
;; 		 (org-export-string-as
;; 		  (org-babel-trim
;; 		   (funcall bhook body (if (eq fmt 'html) 'org 'ascii)))
;; 		  (if (eq fmt 'html) 'org 'ascii) t)
;; 		 html)
;; 		(mapconcat 'identity images "\n")))))
;;     (mapc #'mml-attach-file files)))
