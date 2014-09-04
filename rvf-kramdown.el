(org-export-define-derived-backend 'kramdown 'md
  :translate-alist '(
		     (src-block . org-kramdown-src-block)
		     )
  )

(defun org-kramdown-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into kramdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (eq backend 'md)
    (let (
  	  (lang (org-element-property :language info))
  	  (value (org-element-property :value info)))
      (message "Language is %s. Value is %s" lang value)
      (when (eq lang "sage" )
        (format
         "<div class=\"sage\">
         <script type=\"text/x-sage\">%s</script>
         </div>\n"
         value
         ))
      )))
