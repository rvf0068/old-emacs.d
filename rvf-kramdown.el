(org-export-define-derived-backend 'kramdown 'md
  :translate-alist '(
		     (latex-environment . org-kramdown-latex-environment)
		     (latex-fragment . org-kramdown-latex-fragment)
		     (src-block . org-kramdown-src-block)
		     (template . org-kramdown-template)
		     )
  :options-alist '(
		   (:published "PUBLISHED" nil "true")
		   )
)

(defun org-kramdown-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into kramdown format.
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
      (format
       "``` %s %s\n%s```"
       lang name value)
      )))

(defun org-kramdown-latex-environment (latex-environment contents info)
  (let* (
	 (value (org-remove-indentation
		 (org-element-property :value latex-environment)))
	 (replaced (replace-regexp-in-string
		    "\\\\begin{displaymath}\\|\\\\end{displaymath}" "$$" value))
	 )
    replaced
    ))

(defun org-kramdown-latex-fragment (latex-fragment contents info)
  (let* (
	 (latex (org-element-property :value latex-fragment))
	 (inline-latex (replace-regexp-in-string "\\\\(\\|\\\\)" "$$" latex))
	 (bars-removed (replace-regexp-in-string "|" "\\\\vert " inline-latex))
	 )
    ;; (message "Latex-fragment is %s" latex)
    bars-removed
    ))

;; Transforms
;; ![img](mosaicos.png) to
;; {% img center /images/mosaicos.png %}
;; This was very useful
;; http://stackoverflow.com/questions/16241957/how-can-i-regexp-replace-a-string-in-an-elisp-function

(defun org-kramdown-template (contents info)
  (let* ((title (or (car (plist-get info :title)) ""))
	 (date (or (car (plist-get info :date)) ""))
	 (time "")
	 (keywords (plist-get info :keywords))
	 (published (plist-get info :published))
	 (body (replace-regexp-in-string "## $" "" contents))
	 (images (replace-regexp-in-string
		  "!\\[img\\](\\(.*\\)\.\\(png\\|jpg\\|jpeg\\))"
		  "{% img center /images/\\1.\\2 %}"
		  body))
	 (frontmatter
	  "---\nlayout: %s\ntitle: %s\ndate: %s %s\ncomments: true\npublished: %s\ncategories: %s\n---\n\n"))
    (if keywords ;; if it has keywords, then it is not a page
        (concat (format frontmatter "post" title date time published keywords) images)
      (if org-octopress-is-post
	  (concat (format frontmatter "post" title date time published "") images)
        (concat (format frontmatter "page" title date time published "") images)
	)
      )))

(defun org-kramdown-export-as-kramdown
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'kramdown "*Org KRAMDOWN Export*"
    async subtreep visible-only body-only ext-plist (lambda () (markdown-mode))))

(defun org-kramdown-publish-to-kramdown (plist filename pub-dir)
  (org-publish-org-to 'kramdown filename ".md" plist pub-dir))
