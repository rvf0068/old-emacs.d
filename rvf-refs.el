;; Despite what the docstring says, this opens the file searched
(defun org-mode-reftex-search ()
  "Jump to the notes for the paper pointed to at from reftex search."
  (interactive)
  (setq reftex-default-bibliography '("~/Dropbox/texmf/bibtex/bib/misc/rvf.bib"))
  (org-open-link-from-string
   ;(concat "[[ref:" (format "%s" (reftex-citation t)) "]]")
   (concat "[[ref:" (substring (format "%s" (reftex-citation t)) 1 -1) "]]")
   (setq reftex-default-bibliography nil)
   ))

;; (defadvice reftex-format-citation (before eval-citation-format)
;;   (setq format (eval format)))

(defun org-mode-reftex-setup ()
  (interactive)
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
	 ;; enable auto-revert-mode to update reftex when bibtex file changes on disk
	 ;; (global-auto-revert-mode t)
	 (reftex-parse-all)
	 ;; add a custom reftex cite format to insert links
	 (reftex-set-cite-format
	  '((?b . "[[bib:%l][%l-bib]]")
	    (?p . "[[ref:%l][%l]]")
	    (?q . "[[ref:%l][%l]] (%2a, %y - %t)")
	    (?n . "[[notes:%l][%l-notes]]")
	    (?t . "%t")
	    (?h . "*** %2a, %y - %t\n%j, %v%u, pp. %p\n\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[ref:%l][%l]]")
                )
            )))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search)
  )

;(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; here we should add to the list, not setting it
(setq org-link-abbrev-alist
      '(("bib" . "~/Dropbox/texmf/bibtex/bib/misc/rvf.bib::%s")
	("notes" . "~/Dropbox/org/math/lit.org::#%s")
        ("ref" . org-reflibrary-determine-link)
        ))

;; Possible names
;; reference.pdf
;; reference-djvu.djvu
;; reference-big.pdf
;; reference-big-djvu.djvu

(defun org-reflibrary-determine-link (tag)
  "Map the bibtex key to a file"
  (cond
   ((and (>= (length tag) 5) (equal (substring tag -5 nil) "-djvu"))
    (if
	(and (>= (length tag) 9) (equal (substring tag -9 -4) "-big-"))
	(concat "~/Documents/References/" tag ".djvu")
      (concat "~/Dropbox/References/" tag ".djvu")))

   ((and (>= (length tag) 5) (equal (substring tag -5 nil) "-epub"))
    (if
	(and (>= (length tag) 9) (equal (substring tag -9 -4) "-big-"))
	(concat "~/Documents/References/" tag ".epub")
      (concat "~/Dropbox/References/" tag ".epub")))
   
   ((and (>= (length tag) 4) (equal (substring tag -4 nil) "-big"))
    (concat "~/Documents/References/" tag ".pdf"))
   (t (concat "~/Dropbox/References/" tag ".pdf"))
   )
  )

(defun org-reflibrary-movebibsnarf()
  (interactive)
  (set-buffer "rvf.bib")
  (goto-char (point-max))
  (set-buffer "*bibsnarfed record*")
  (append-to-buffer "rvf.bib" (point-min) (point-max))
  (switch-to-buffer "rvf.bib")
  )

(global-set-key [(f5)] 'org-reflibrary-movebibsnarf)
