(load "~/.emacs.d/rvf-elpa.el")

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

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/org-mode/doc")))

(defun make-backup-file-name (file-name)
  "Create the non-numeric backup file name for `file-name'."
  (require 'dired)
  (if (file-exists-p "~/backups")
      (concat (expand-file-name "~/backups/")
              (dired-replace-in-string "/" "-" file-name))
    (concat file-name "~")))

;; emacs-lisp

(define-key emacs-lisp-mode-map [(tab)] 'lisp-complete-symbol)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)

;; bibretrieve
(require 'bibretrieve)
(setq bibretrieve-backends ' (("mrl" . 10)))

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

(setq cdlatex-math-modify-alist
      '(
	( ?B    "\\mathbb"           nil        t   nil nil )
	)
      )

(setq cdlatex-simplify-sub-super-scripts nil)

;; markdown

(defun my-markdown-mode-hook()
  (local-set-key (kbd "$")
                 (lambda () (interactive)
                   (insert "$$$$")
                   (forward-char -2)))
  (setq markdown-enable-math t)
  )

(add-hook 'markdown-mode-hook 'turn-on-org-cdlatex)
(add-hook 'markdown-mode-hook 'smartparens-mode)
(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

;; yasnippet: https://github.com/capitaomorte/yasnippet

(require 'yasnippet) 
(yas--load-snippet-dirs)
;; otherwise some lines are indented after expansion of a snippet
(setq yas/indent-line 'fixed)

;; octopress
(require 'octopress)

;; smart-mode-line: https://github.com/Bruce-Connor/smart-mode-line

(setq sml/theme 'dark)
(require 'smart-mode-line)
(sml/setup)

;; smartparens: https://github.com/Fuco1/smartparens

(require 'smartparens)
(sp-with-modes '(
                 org-mode
                 markdown-mode
                 )
  (sp-local-pair "`" nil :actions nil)
  (sp-local-pair "'" nil :actions nil)
;  (sp-local-pair "\\[" "\n\n    \\]")
  (sp-local-pair "\\{" "\\}")
  )
(sp-with-modes '(
		 emacs-lisp-mode
                 )
  (sp-local-pair "'" nil :actions nil)
  )
(show-smartparens-global-mode +1)

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

(require 'calfw)
(require 'calfw-org) 
(require 'cal-catholic)
(require 'cal-benedictine)
(require 'cal-dominican)
(require 'cal-franciscan)
(require 'cal-sanctoral-updates)
(add-hook 'diary-display-hook 'fancy-diary-display)
(setq diary-list-include-blanks t)

(load "~/.emacs.d/rvf-misc.el")

(load "~/.emacs.d/rvf-key-bindings.el")

(load "~/.emacs.d/rvf-cosmetic.el")

(load "~/.emacs.d/rvf-appts.el")

(load "~/.emacs.d/rvf-refs.el")

(if (or (equal system-name "lahp") (equal system-name "ladell"))
    (load "~/.emacs.d/rvf-personal.el")
  )

(diary)
