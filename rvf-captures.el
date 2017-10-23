;; from https://stackoverflow.com/a/46685539/577007
(defun transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
  (mapcar #'(lambda (c) (if (equal c ?[) ?\( (if (equal c ?]) ?\) c))) string-to-transform))
  )


;; code by Adam Porter, see https://lists.gnu.org/archive/html/emacs-orgmode/2016-09/msg00205.html

(defun url-to-org-with-readability (url)
  "Get page content of URL with python-readability, convert to
Org with Pandoc, and display in buffer."

  (let (title content new-buffer)

    (with-temp-buffer
      (unless (= 0 (call-process "python" nil '(t t) nil "-m" "readability.readability" "-u" url))
        (error "Python readability-lxml script failed: %s" (buffer-string)))

      ;; Get title
      (goto-char (point-min))
      (setq title (buffer-substring-no-properties (search-forward "Title:") (line-end-position)))

      (unless (= 0 (call-process-region (point-min) (point-max) "pandoc" t t nil "--wrap=auto" "-f" "html" "-t" "org"))
        (error "Pandoc failed."))
      (setq content (buffer-substring (point-min) (buffer-end 1))))

    ;; Make new buffer
    (setq new-buffer (generate-new-buffer title))
    (with-current-buffer new-buffer
      (insert (concat "* [[" url "][" title "]]\n\n"))
      (insert content)
      (org-mode)
      (goto-char (point-min))
      (org-cycle)
      (switch-to-buffer new-buffer))))

;; derived from http://emacs.stackexchange.com/a/3990/29
(defun my-yank-org-link (text)
    (string-match org-bracket-link-regexp text)
    (substring text (match-beginning 1) (match-end 1)))

(defun my-yank-org-description (text)
    (string-match org-bracket-link-regexp text)
    (substring text (match-beginning 3) (match-end 3)))

(defun read-url-with-org ()
  "Call `url-to-org-with-readability' on URL in kill ring."
  (interactive)
  (url-to-org-with-readability (my-yank-org-link (first kill-ring))))

(defun org-capture-web-page-with-readability (&optional url)
  "Return string containing entire capture to be inserted in org-capture template."
  (let ((url (my-yank-org-link (first kill-ring)))
        ;; From org-insert-time-stamp
        (timestamp (format-time-string (concat "[" (substring (cdr org-time-stamp-formats) 1 -1) "]")))
        title title-linked content)

    (with-temp-buffer
      (unless (= 0 (call-process "python" nil '(t t) nil "-m" "readability.readability" "-u" url))
        (error "Python readability-lxml script failed: %s" (buffer-string)))

      ;; Get title
      (goto-char (point-min))
      (setq title (buffer-substring-no-properties (search-forward "Title:") (line-end-position)))
      (setq title-linked (concat "[[" url "][" title "]]"))

      (unless (= 0 (call-process-region (point-min) (point-max) "pandoc" t t nil "--no-wrap" "-f" "html" "-t" "org"))
        (error "Pandoc failed."))

      ;; Demote page headings in capture buffer to below the
      ;; top-level Org heading and "Article" 2nd-level heading
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (rx bol (1+ "*") (1+ space)) nil t)
          (beginning-of-line)
          (insert "**")
          (end-of-line)))

      (goto-char (point-min))
      (goto-line 2)
      (setq content (s-trim (buffer-substring (point) (buffer-end 1))))

      ;; Return capture for insertion
      (concat title-linked " :website:\n\n" timestamp "\n\n** Article\n\n" content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-capture-template-for-org-board ()
  "Return string containing entire capture to be inserted in org-capture template."
  (let ((url (my-yank-org-link (first kill-ring)))
	(title (my-yank-org-description (first kill-ring)))
        )

      (concat title "\n"
	      "  :PROPERTIES:\n"
	      "  :URL: " url "\n"
	      "  :WGET_OPTIONS: --recursive -l 1\n"
	      "  :END:" )
    ))

(defun rvf/org-capture-protocol (keys description file)
  "Using protocols. Tags"
  `(,keys ,description entry
          (file ,file)
"* [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]] %^g\n   %U\n%?" :empty-lines 0))

(defun rvf/org-capture-protocol-tags-immediate (keys description file)
  "Using protocols, immediate, tags"
  `(,keys ,description entry
          (file ,file)
"* [[%:link][%:description]] %^g\n   %U\n%?" :immediate-finish t :empty-lines 0))

(defun rvf/org-capture-misc (keys description file)
  "Misc capture, asking for title."
  `(,keys ,description entry
          (file ,file)
"* %^{Title}\n   %u\n\n%?" :empty-lines 0))

(defun rvf/org-capture-todo (keys description file)
  "Misc capture, asking for title."
  `(,keys ,description entry
          (file+headline ,file "Tasks")
"* TODO %^{Task}\n   %u\n\n%?" :empty-lines 0))

(defun rvf/org-capture-template-project (project key filename)
  "Generic Capture"
  `((,key ,project)
    (,(concat key "g") ,(concat project " in gnus") entry
     (file ,(concat "~/Dropbox/org/" filename))
     "* %:subject\n\n  %U\n%a\n%:group\n\n%i" :empty-lines 0)
    (,(concat key "w") ,(concat project " in web") entry
     (file ,(concat "~/Dropbox/org/" filename))
     "* [[%:link][%:description]]\n  %U\n%?" :immediate-finish t :empty-lines 0)
    (,(concat key "z") ,(concat project " misc") entry
     (file ,(concat "~/Dropbox/org/" filename))
     "* %^{Title}\n  %u\n\n%?" :empty-lines 0)
  ))

(setq org-capture-templates
      `(
        ("a" "Email" entry
         (file "~/Dropbox/org/misc.org") 
         "* TODO Respond to [[%l][%:fromname]] %^g:email\n\n%?")
	("c" "Web site"
	 entry
	 (file "~/Dropbox/org/misc.org")
	 "* [[%:link][%:description]] :website:\n%U %?%:initial")
        ,(rvf/org-capture-protocol-tags-immediate
          "b" "Bookmark" "~/Dropbox/org/bookmarks.org")
        ,(rvf/org-capture-protocol-tags-immediate
          "d" "Downloads" "~/Dropbox/org/downloads.org")
        ,@(rvf/org-capture-template-project "Emacs" "e" "emacs.org")        
        ,@(rvf/org-capture-template-project "LaTeX" "l" "latex.org")        
        ,@(rvf/org-capture-template-project "Math" "m" "mathnotes.org")        
        ,@(rvf/org-capture-template-project "Org Mode" "o" "orgmodec.org")
        ,(rvf/org-capture-protocol
          "p" "Papers" "~/Dropbox/org/papers.org")       
        ,(rvf/org-capture-todo
          "t" "Todo" "~/Dropbox/org/misctodo.org")     
        ,@(rvf/org-capture-template-project "Ubuntu" "u" "ubuntu.org")
        ,(rvf/org-capture-misc
          "v" "Misc" "~/Dropbox/org/misc.org")  
        ,(rvf/org-capture-protocol
          "z" "Misc from web" "~/Dropbox/org/misc.org")
	;; ("w" "Use python-readability" entry
	;;  (file "~/Dropbox/org/notes.org")
	;;  "* %(org-capture-web-page-with-readability)")
	("w" "Web site" entry
  (file "")
  "* %a :website:\n\n  %U %?\n\n%:initial")
	("k" "Template for org-board" entry (file "~/Dropbox/org/board.org")
	 "* %(org-capture-template-for-org-board)")
	))
