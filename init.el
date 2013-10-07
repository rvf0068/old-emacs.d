;; From http://www.emacswiki.org/emacs/LoadPath#AddSubDirectories
;; to add a directory and its subdirectories

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; From https://github.com/magit/magit/blob/maint/INSTALL.md#installing-from-git
;; to add info documentation to index

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/magit")))

(require 'magit)

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/org-mode/doc")))

;; cdlatex: http://staff.science.uva.nl/~dominik/Tools/cdlatex/

(setq cdlatex-math-symbol-alist
   '(
     ( ?c  ("\\colon"))
     ( ?-  ("\\cap" "\\leftrightarrow" "\\longleftrightarrow" ))
     ( ?.  ("\\ldots" "\\cdots" "\\cdot" ))
     ( ?<  ("\\leq" ))
     ( ?>  ("\\geq" ))
     ( 123  ("\\{ \\}" ))
     ( 125  ("\\subseteq" ))
     ( ?\[  ("\\subseteq" ))
     )
   )

;; yasnippet: https://github.com/capitaomorte/yasnippet

(require 'yasnippet) 
;; (setq yas-snippet-dirs '(
;;                          "~/Dropbox/emacs/site-lisp/yasnippet-rvf/yasnippets/"
;;                          "~/Dropbox/emacs/site-lisp/yasnippet/snippets"
;;                          ))
(yas--load-snippet-dirs)
;; otherwise some lines are indented after expansion of a snippet
(setq yas/indent-line 'fixed)

;; smartparens: https://github.com/Fuco1/smartparens

(require 'smartparens)
(sp-with-modes '(
                 org-mode
                 markdown-mode
                 )
  (sp-local-pair "`" nil :actions nil)
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "$" nil :actions nil)
  (sp-local-pair "\\[" "\\]")
  )

;; smex: https://github.com/nonsequitur/smex/

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(load "~/.emacs.d/rvf-settings.el")

;; this is needed since LaTeX-mode has not been loaded (being an ELPA package)
(add-hook 'LaTeX-mode-hook (lambda () (load "~/.emacs.d/rvf-latex.el")))

(load "~/.emacs.d/rvf-org.el")

(load "~/.emacs.d/rvf-personal.el")
