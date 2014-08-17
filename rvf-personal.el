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
                      ("Maestría" . (:foreground "magenta" :weight bold :underline t))
                      ("titulacion" . (:foreground "blue" :weight bold :underline t))
                      ("todo" . (:foreground "orange" :weight bold :underline t))
                      ))

(setq org-agenda-category-icon-alist
      '(
        ("Tasks" "/usr/share/icons/Humanity/stock/16/stock_task.svg" nil nil :ascent center)
        ("LIMA" "/usr/share/icons/Humanity/actions/16/add.svg" nil nil :ascent center)
        ("WorldCup" "/usr/share/icons/Humanity/categories/16/gnome-globe.svg" nil nil :ascent center)
        ("Math" "/usr/share/icons/gnome/16x16/apps/libreoffice-math.png" nil nil :ascent center)
        ("Saint" "/usr/share/icons/gnome/16x16/emotes/face-angel.png" nil nil :ascent center)
        ("Emacs" "/usr/share/emacs/24.3/etc/images/icons/hicolor/16x16/apps/emacs.png" nil nil :ascent center)
        ("Clase" "/usr/share/icons/gnome/16x16/mimetypes/libreoffice-formula.png" nil nil :ascent center)
	("" '(space . (:height (16) :width (16))))
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

;; from http://www.railsonmaui.com/blog/2014/03/05/octopress-setup-with-github-and-org-mode-v2/

(setq my-common-octopress-settings
      '(:base-extension "org"
                        ;; :with-toc t
                        ;; :with-sub-superscript nil
			:exclude "options.org\\|opciones.org\\|exercises.*org"
                        :recursive t
			:author nil
                        ;; :headline-levels 4
                        ;; :body-only t
			))

;; (setq my-static-directories '("about" "meta" "tips"))
(setq my-static-directories '("page"))

(setq my-base-directory "~/Dropbox/paginas/sistemas-dinamicos/source/")

(defun my-create-octopress-static (prj)
  (let ((base-dir (expand-file-name prj)))
    `(,prj . (:base-directory ,base-dir
                              :publishing-directory ,base-dir
			      :publishing-function org-md-publish-to-md
                              ,@my-common-octopress-settings))))

(defun my-static-components ()
  (mapcar 'my-create-octopress-static my-static-directories))

(let ((default-directory my-base-directory))
(setq org-publish-project-alist
      `(
        ;; components
        ;; ("blog" . (:components ("blog-org" "blog-extra" "about" "meta" "tips")))
        ("blog" . (:components ("blog-org" "blog-extra" "blog-pdf" "index")))

	;; home page
	("index" . (:base-directory ,(expand-file-name "index")
				    :publishing-directory ,(expand-file-name ".")
				    :publishing-function org-md-publish-to-md
				    ,@my-common-octopress-settings))

        ;; blog articles
        ("blog-org" .  (:base-directory ,(expand-file-name "org")
                                        :publishing-directory ,(expand-file-name "_posts")
					:section-numbers nil
					:publishing-function org-md-publish-to-md
                                        ,@my-common-octopress-settings))
        ("blog-pdf" .  (:base-directory ,(expand-file-name "org")
                                        :publishing-directory ,(expand-file-name "org")
					:publishing-function org-beamer-publish-to-pdf
                                        ,@my-common-octopress-settings))
        ("blog-extra" . (:base-directory ,(expand-file-name "org")
                                         :publishing-directory ,(expand-file-name "images")
                                         :base-extension "css\\|png\\|jpg\\|gif\\|svg"
                                         :publishing-function org-publish-attachment
                                         :recursive t))

        ;; static articles
        ,@(my-static-components))))

;; This works for "sistemas-dinamicos". Clearly will have to be modified somehow
(add-to-list 'org-link-abbrev-alist
	     '("pres" . "https://github.com/rvf0068/sistemas-dinamicos/blob/gh-pages/org/%s.pdf?raw=true"))

(load "~/.emacs.d/rvf-captures.el")

(setq diary-file "~/Dropbox/emacs/diary")
(setq calendar-latitude [20 7 north])
(setq calendar-longitude [98 43 west])
(setq calendar-location-name "Pachuca, Hidalgo, Mexico")

;; see http://thread.gmane.org/gmane.emacs.gnus.general/82879/focus=83006
;;(add-to-list 'load-path "~/Dropbox/emacs/site-lisp/gnus/lisp")
(push "~/Dropbox/emacs/site-lisp/gnus/lisp" load-path)
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
;; see https://github.com/tmalsburg/helm-bibtex/commit/ab85b12d6b3cdb4745b73e7f67f426ff139a8b3c
(setq helm-bibtex-pdf-open-function
      (lambda (fpath) (shell-command-to-string
		       (concat "/usr/bin/evince " fpath " &"))))

(setq org2blog/wp-blog-alist
      '(("hgomat"
         :url "http://hgomat.wordpress.com/xmlrpc.php"
         :username "rvf0068"
         :tags-as-categories nil)))

;; do not show holidays in diary
(setq diary-show-holidays-flag nil)
