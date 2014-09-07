(org-export-define-derived-backend 'kramdown 'md
  :translate-alist '(
		     (src-block . org-kramdown-src-block)
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
	)
      ))
(defun org-kramdown-export-as-kramdown
   (&optional async subtreep visible-only body-only ext-plist)
   (interactive)
   (org-export-to-buffer 'kramdown "*Org KRAMDOWN Export*"
     async subtreep visible-only body-only ext-plist (lambda () (markdown-mode))))
