;; identity
(setq user-full-name "Rafael Villarroel")
(setq user-mail-address "rafaelv@uaeh.edu.mx")

;; from https://github.com/magnars/.emacs.d/blob/master/init.el
;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; org stuff
(setq org-agenda-files "~/Dropbox/org/agenda/agendafiles")

(setq org-tag-faces '(
                      ("LIMA" . (:foreground "green" :weight bold :underline t))
                      ("titulacion" . (:foreground "blue" :weight bold :underline t))
                      ("todo" . (:foreground "orange" :weight bold :underline t))
                      ))

(setq org-agenda-category-icon-alist
      '(
        ("Tasks" "/usr/share/icons/Humanity/stock/16/stock_task.svg" nil nil :ascent center)
        ("LIMA" "/usr/share/icons/Humanity/actions/16/add.svg" nil nil :ascent center)
        ("Math" "/usr/share/icons/gnome/16x16/apps/libreoffice-math.png" nil nil :ascent center)
        ("Saint" "/usr/share/icons/gnome/16x16/emotes/face-angel.png" nil nil :ascent center)
        ("Emacs" "/usr/share/emacs/24.3/etc/images/icons/hicolor/16x16/apps/emacs.png" nil nil :ascent center)
        ))

(setq org-publish-project-alist
      '(
	("topology-org" .  (
			    :base-directory "~/Dropbox/paginas/topology/source/org"
					    :base-extension "org"
					    :exclude "options.org\\|exercises.*org"
					    :publishing-directory "~/Dropbox/paginas/topology/source/_posts/"
					    :publishing-function org-md-publish-to-md
					    ))
	("topology-pdf" .  (
			    :base-directory "~/Dropbox/paginas/topology/source/org"
					    :base-extension "org"
					    :exclude "options.org\\|exercises.org"
					    :publishing-directory "~/Dropbox/paginas/topology/source/org/"
					    :publishing-function org-beamer-publish-to-pdf
					    ))
        ;; ("blog-extra" . (:base-directory "~/git/blog/source/org_posts/"
        ;;                                  :publishing-directory "~/git/blog/source/"
        ;;                                  :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|svg"
        ;;                                  :publishing-function org-publish-attachment
        ;;                                  :recursive t
        ;;                                  :author nil
        ;;                                  ))
        ("topology" . (:components ("topology-org" "topology-pdf")))
	("galois-org" .  (
			    :base-directory "~/Dropbox/paginas/galois/source/org"
					    :base-extension "org"
					    :exclude "opciones.org\\|ejercicios.org"
					    :publishing-directory "~/Dropbox/paginas/galois/source/_posts/"
					    :publishing-function org-md-publish-to-md
					    ))
	("galois-pdf" .  (
			    :base-directory "~/Dropbox/paginas/galois/source/org"
					    :base-extension "org"
					    :exclude "opciones.org\\|ejercicios.org"
					    :publishing-directory "~/Dropbox/paginas/galois/source/org/"
					    :publishing-function org-beamer-publish-to-pdf
					    ))
        ("galois" . (:components ("galois-org" "galois-pdf")))
        ))

(load "~/.emacs.d/rvf-captures.el")

(setq diary-file "~/Dropbox/emacs/diary")
(setq calendar-latitude [20 7 north])
(setq calendar-longitude [98 43 west])
(setq calendar-location-name "Pachuca, Hidalgo, Mexico")

(add-to-list 'load-path "~/Dropbox/emacs/site-lisp/gnus/lisp")
(setq gnus-home-directory "~/Dropbox/gnus")
(setq gnus-directory "~/Dropbox/gnus/News")
(setq gnus-cache-directory "~/Dropbox/gnus/News/cache/")
(setq message-directory "~/Dropbox/gnus/Mail")

(setq bbdb-file "~/Dropbox/gnus/.bbdb")

(setq gnus-always-read-dribble-file t)


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
 (kbd "<f12> g")
 (lambda nil (interactive)
   (dired "~/Dropbox/paginas/galois/source/org/")
   ))
(global-set-key
 (kbd "<f12> i")
 (lambda nil (interactive)
   (find-file "~/.emacs.d/init.el")
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
   (dired "~/Dropbox/paginas/topology/source/org/")
   ))
;; (global-set-key
;;  (kbd "<f12> t")
;;  (lambda nil (interactive)
;;    (find-file "~/Dropbox/texmf/tex/latex/uaeh/titulacion.org")
;;    ))

;; anything-books
;; the entry point is M-x helm-books-command
(require 'helm)
(require 'anything-books)
(setq abks:books-dir '("~/Dropbox/References" "~/Documents/References"))
(setq abks:open-command "evince")

(autoload 'helm-bibtex "helm-bibtex" "" t)
(setq helm-bibtex-bibliography '("/home/rafael/Dropbox/texmf/bibtex/bib/misc/rvf.bib"))
(setq helm-bibtex-library-path "/home/rafael/Dropbox/References/")
(setq helm-bibtex-notes-path "/home/rafael/Downloads/scratch/")

