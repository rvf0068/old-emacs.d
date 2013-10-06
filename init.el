;; From http://www.emacswiki.org/emacs/LoadPath#AddSubDirectories
;; to add a directory and its subdirectories

(let ((default-directory "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; From https://github.com/magit/magit/blob/maint/INSTALL.md#installing-from-git
;; to add info documentation to index

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/site-lisp/magit$")))

(require 'magit)
