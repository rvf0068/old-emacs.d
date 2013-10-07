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

(load "~/.emacs.d/rvf-settings.el")

;; this is needed since LaTeX-mode has not been loaded (being an ELPA package)
(add-hook 'LaTeX-mode-hook (lambda () (load "~/.emacs.d/rvf-latex.el")))
