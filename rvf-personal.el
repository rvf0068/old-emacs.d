;; identity
(setq user-full-name "Rafael Villarroel")
(setq user-mail-address "rafaelv@uaeh.edu.mx")

(setq org-agenda-files "~/Dropbox/org/agenda/agendafiles")

(setq diary-file "~/Dropbox/emacs/diary")
(setq calendar-latitude [20 7 north])
(setq calendar-longitude [98 43 west])
(setq calendar-location-name "Pachuca, Hidalgo, Mexico")

(global-set-key
 (kbd "<f12> b")
 (lambda nil (interactive)
   (find-file "~/Dropbox/texmf/bibtex/bib/misc/rvf.bib")
   ))
(global-set-key
 (kbd "<f12> c")
 (lambda nil (interactive)
   (find-file "~/Dropbox/org/cheatsheet/cheatsheetmisc.org")
   ))
(global-set-key
 (kbd "<f12> d")
 (lambda nil (interactive)
   (find-file "~/Dropbox/emacs/dotemacs/dotemacs.org")
   ))
(global-set-key
 (kbd "<f12> k")
 (lambda nil (interactive)
   (switch-to-buffer "*Bookmark List*")
   ))
(global-set-key
 (kbd "<f12> l")
 (lambda nil (interactive)
   (find-file "~/Dropbox/org/math/lit.org")
   ))
(global-set-key
 (kbd "<f12> r")
 (lambda nil (interactive)
   (find-file "~/Dropbox/org/remember.org")
   ))
(global-set-key
 (kbd "<f12> s")
 (lambda nil (interactive)
   (find-file "~/Dropbox/emacs/site-lisp/scratch.el")
   ))
(global-set-key
 (kbd "<f12> t")
 (lambda nil (interactive)
   (find-file "~/Dropbox/texmf/tex/latex/uaeh/titulacion.org")
   ))
