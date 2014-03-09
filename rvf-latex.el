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

(abbrev-mode)

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

;; some functions

(defun split-for-tikz2pdf ()
  (interactive)
  (TeX-run-style-hooks "tikz")
  (TeX-run-style-hooks "tkz-berge")
  (TeX-run-style-hooks "tkz-berge-add")
  (split-window-horizontally 60)
  (other-window 1)
  (split-window-vertically 15)
  (sit-for 4)
  (find-file "tikz2pdf_temp.pdf")
  ;;(doc-view-mode 1)
  (auto-revert-mode 1)
  (other-window 1)
  (let ((file-path (buffer-file-name)))
    (when file-path
      (shell)
      (comint-send-string (get-buffer-process (current-buffer))
                          (format "tikz2pdf -v %S \n" file-path))))
  )

(defun load-tikz ()
  (interactive)
  (TeX-run-style-hooks "tikz")
  (TeX-run-style-hooks "tkz-berge")
  (TeX-run-style-hooks "tkz-berge-add")
  )
