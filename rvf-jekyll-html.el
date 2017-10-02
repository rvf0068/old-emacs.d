(org-export-define-derived-backend 'jekyll-html 'html
  :translate-alist '(
		     (latex-environment . org-jekyll-html-latex-environment)
		     ;; (latex-fragment . org-jekyll-latex-html-fragment)
		     (src-block . org-jekyll-html-src-block)
		     (inner-template . org-jekyll-html-template)
		     )
  :options-alist '(
		   (:published "PUBLISHED" nil "true")
		   (:layout "LAYOUT" nil "post")
		   )
)

(defun org-jekyll-html-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into jekyll format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((lang (org-element-property :language src-block))
	(value (org-element-property :value src-block))
	(name (or (org-element-property :name src-block) ""))
	)
    (if (string= lang "sage")
        (format
         "<div class=\"sage\"><script type=\"text/x-sage\">\n%s</script></div>\n"
         value)
      ;; (format
      ;;  "``` %s %s\n%s```"
      ;;  lang name value)
      (format
       "{%% highlight %s %%}\n %s{%% endhighlight %%}"
       lang value)
      )))

(defun org-jekyll-html-latex-environment (latex-environment contents info)
  (let* (
	 (value (org-remove-indentation
		 (org-element-property :value latex-environment)))
	 (replaced (replace-regexp-in-string
		    "\\\\begin{displaymath}\\|\\\\end{displaymath}" "$$" value))
	 )
    replaced
    ))

(defun org-jekyll-html-latex-fragment (latex-fragment contents info)
  (let* (
	 (latex (org-element-property :value latex-fragment))
	 (inline-latex (replace-regexp-in-string "\\\\(\\|\\\\)" "$$" latex))
	 (bars-removed (replace-regexp-in-string "|" "\\\\vert " inline-latex))
	 )
    ;; (message "Latex-fragment is %s" latex)
    bars-removed
    ))

;; We have to transform
;; <img src="recta.png" alt="recta" width="400" align="center" />
;; to
;; <img src="{{ site.baseurl}}/images/recta.png" alt="recta" width="400" align="center" />
;; TODO: Deal with character "

(defun org-jekyll-html-template (contents info)
  (let* ((title (or (car (plist-get info :title)) ""))
	 (date (or (car (plist-get info :date)) ""))
	 (time "")
	 (keywords (or (plist-get info :keywords) ""))
	 (published (plist-get info :published))
	 (layout (plist-get info :layout))
	 (body (replace-regexp-in-string "#+ $" "" contents))
	 (images (replace-regexp-in-string
		  "<img src=\\(.\\)\\(.*\\)\.\\(png\\|jpg\\|jpeg\\)"
		  "<img src=\\1{{ site.baseurl }}/images/\\2.\\3"
		  body))
	 (corollaries (replace-regexp-in-string
		       ":B<sub>corollary</sub>:"
		       "Corolario"
		       images))
	 (theorems (replace-regexp-in-string
		       ":B<sub>theorem</sub>:"
		       "Teorema"
		       corollaries))
	 (lemmas (replace-regexp-in-string
		       ":B<sub>lemma</sub>:"
		       "Lema"
		       theorems))
	 (columns (replace-regexp-in-string
		   ":BMCOL:"
		   ""
		   lemmas))
	 (blocks (replace-regexp-in-string
		   ":B<sub>block</sub>"
		   ""
		   columns))
	 (morecols (replace-regexp-in-string
		   ":B<sub>column</sub><br />"
		   ""
		   blocks))
	 (ignoreh (replace-regexp-in-string
		    ":B<sub>ignoreheading</sub>:<br />"
		    ""
		    morecols))
	 (lists (replace-regexp-in-string
		 "\n\n    -"
		 "
    -"
		 ignoreh
		 ))
	 (frontmatter
	  "---\nlayout: %s\ntitle: %s\ndate: %s %s\ncomments: true\npublished: %s\ncategories: %s\n---\n\n"))
    (concat (format frontmatter layout title date time published keywords) lists)
    ))

(defun org-jekyll-html-export-as-jekyll
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'jekyll-html "*Org JEKYLL-HTML Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))

(defun org-jekyll-html-publish-to-jekyll (plist filename pub-dir)
  (org-publish-org-to 'jekyll-html filename ".html" plist pub-dir))
