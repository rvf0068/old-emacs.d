(setq TeX-parse-self t)  ; Enable parse on load
(setq TeX-auto-save t)  ; Enable parse on save
(setq TeX-electric-escape t)
(setq TeX-insert-braces nil)
(setq-default TeX-PDF-mode t)
(setq TeX-math-close-single-dollar t)

(font-latex-add-keywords '(("alert" "{")) 'italic-command)
(setq font-latex-fontify-sectioning 1.15)

(LaTeX-add-environments
 '("definition")
 '("theorem")
 '("corollary"))

(LaTeX-math-mode)

;; reftex

(turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-use-external-file-finders t)
(setq reftex-toc-split-windows-horizontally t)
(add-to-list 'reftex-section-levels '("frametitle" . 4))

;; key bindings

(local-set-key (kbd "\C-cc")
               (lambda () (interactive)
                 (insert "\\colon ")))
(local-set-key (kbd "^")
               (lambda (superscript) (interactive "sSuperscript: ")
                 (expand-abbrev)
                 (insert (concat "^{" superscript "}"))
                 (if (equal superscript "") (forward-char -1))
                 ))
(local-set-key (kbd "_")
               (lambda (subscript) (interactive "sSubscript: ")
                 (expand-abbrev)
                 (insert (concat "_{" subscript "}"))
                 (if (equal subscript "") (forward-char -1))
                 ))
(local-set-key [(tab)] 'dabbrev-expand)

;; TeX-command-list

(add-to-list 'TeX-command-list
           '("Acrobat"
             "acroread %s.pdf"
             TeX-run-discard nil nil) t)
(add-to-list 'TeX-command-list
             '("ps2pdf"
               "ps2pdf %s.ps %s.pdf"
               TeX-run-command nil nil) t)
(add-to-list 'TeX-command-list
             '("dvips"
               "dvips %d -o %f"
               TeX-run-command nil nil) t)
(add-to-list 'TeX-command-list      
             '("Makeindex"
               "makeindex %s"
               TeX-run-command t t) t)
(add-to-list 'TeX-command-list      
             '("Shell escape"
               "pdflatex -shell-escape %s"
               TeX-run-command t t) t)
