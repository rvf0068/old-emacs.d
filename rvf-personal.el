;; identity
(setq user-full-name "Rafael Villarroel")
(setq user-mail-address "rafaelv@uaeh.edu.mx")

;; from https://github.com/magnars/.emacs.d/blob/master/init.el
;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; org stuff
(setq org-agenda-files "~/Dropbox/org/agenda/agendafiles")

(setq org-journal-dir "~/Dropbox/org/journal/")

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

;; org-ref
(require 'org-ref)
(setq org-ref-default-bibliography '("~/Dropbox/texmf/bibtex/bib/misc/rvf.bib"))
(setq org-ref-pdf-directory "~/Dropbox/References/")

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
			:exclude "options.org\\|opciones.org\\|exercises.*org"
                        :recursive t
			:author nil
			))

(setq my-static-directories '("page"))

(defun my-create-octopress-static (prj)
  (let ((base-dir (expand-file-name prj)))
    `(,prj . (:base-directory ,base-dir
                              :publishing-directory ,base-dir
			      :publishing-function org-kramdown-publish-to-kramdown
                              ,@my-common-octopress-settings))))

(defun my-static-components ()
  (mapcar 'my-create-octopress-static my-static-directories))

(defun my-octopress-projects (my-pair)
(let (
      (code (car my-pair))
      (default-directory (nth 1 my-pair))
      )
(setq org-publish-project-alist (nconc org-publish-project-alist
      `(
        ;; components
        (,code . (:components (,(concat code "-org") ,(concat code "-extra") ,(concat code "-pdf") ,(concat code "-index"))))
	;; home page
	(,(concat code "-index") . (:base-directory ,(expand-file-name "index")
				    :publishing-directory ,(expand-file-name ".")
				    :publishing-function org-kramdown-publish-to-kramdown
				    ,@my-common-octopress-settings))
        ;; blog articles
        (,(concat code "-org") .  (:base-directory ,(expand-file-name "org")
                                        :publishing-directory ,(expand-file-name "_posts")
					:section-numbers nil
					:publishing-function org-kramdown-publish-to-kramdown
                                        ,@my-common-octopress-settings))
        (,(concat code "-pdf") .  (:base-directory ,(expand-file-name "org")
                                        :publishing-directory ,(expand-file-name "org")
					:publishing-function org-beamer-publish-to-pdf
                                        ,@my-common-octopress-settings))
        (,(concat code "-extra") . (:base-directory ,(expand-file-name "org")
                                         :publishing-directory ,(expand-file-name "images")
                                         :base-extension "png\\|jpg\\|jpeg"
                                         :publishing-function org-publish-attachment
                                         :recursive t))
        ;; static articles
        ,@(my-static-components))))
))

(setq my-base-directories '(
			    ("sd" "sistemas-dinamicos")
			    ("top" "grad-topology")
			    ("rvf" "rvf0068")
			    ("sage" "ejemplos-sage")
			    ("variable" "variable")
			    ("discretas" "discretas")
			    ))

(defun form-octopress-project (pair)
  (let ((dir (nth 1 pair)))
  (list (car pair) (concat "~/Dropbox/paginas/" dir "/source/"))
  ))

(mapcar 'my-octopress-projects (mapcar 'form-octopress-project my-base-directories))

(setq org-publish-project-alist 
      (nconc org-publish-project-alist
	     '(
	       ("olimpiada-paginas" 
		:base-directory "/home/rafael/Dropbox/paginas/olimpiada/paginas" 
		:publishing-directory "/home/rafael/Dropbox/paginas/olimpiada" 
		:publishing-function org-jekyll-publish-to-jekyll 
		:base-extension "org")
	       ("olimpiada" . (:components ("olimpiada-paginas")))
	       ("topologia" . (:components (
					    "topologia-clases"
					    "topologia-inicio"
					    "topologia-pdf"
					    )))
	       ("topologia-clases"
		:base-directory "/home/rafael/Dropbox/TeXfiles/ClasesPachuca/2016aTopologia/clases"
		:publishing-directory "/home/rafael/Dropbox/paginas/jekyll-sites/topologia/_posts"
		:publishing-function org-jekyll-publish-to-jekyll
		:base-extension "org"
		:exclude "options.org"
		)
	       ("topologia-inicio"
		:base-directory "/home/rafael/Dropbox/TeXfiles/ClasesPachuca/2016aTopologia/inicio"
		:publishing-directory "/home/rafael/Dropbox/paginas/jekyll-sites/topologia/"
		:publishing-function org-jekyll-publish-to-jekyll
		:base-extension "org")
	       ("topologia-pdf"
		:base-directory "/home/rafael/Dropbox/TeXfiles/ClasesPachuca/2016aTopologia/clases"
		:publishing-directory "/home/rafael/Dropbox/paginas/jekyll-sites/topologia/pdfs"
		:publishing-function org-beamer-publish-to-pdf
		:base-extension "org"
		:exclude "options.org"
		)
	       ("geometria" . (:components (
					    "geometria-clases"
					    "geometria-inicio"
					    "geometria-pdf"
					    )))
	       ("geometria-clases"
		:base-directory "/home/rafael/Dropbox/TeXfiles/ClasesPachuca/2016cGeometria/clases"
		:publishing-directory "/home/rafael/Dropbox/paginas/geometria/_posts"
		:base-extension "org"
		:publishing-function org-jekyll-html-publish-to-jekyll
		:body-only t
		:exclude "options.org"
		)
	       ("geometria-inicio"
		:base-directory "/home/rafael/Dropbox/TeXfiles/ClasesPachuca/2016cGeometria/inicio"
		:publishing-directory "/home/rafael/Dropbox/paginas/geometria/"
		:publishing-function org-jekyll-html-publish-to-jekyll
		:body-only t
		:base-extension "org")
	       ("geometria-pdf"
		:base-directory "/home/rafael/Dropbox/TeXfiles/ClasesPachuca/2016cGeometria/clases"
		:publishing-directory "/home/rafael/Dropbox/paginas/geometria/pdfs"
		:publishing-function org-beamer-publish-to-pdf
		:base-extension "org"
		:exclude "options.org"
	       )
	     )))

(load "~/.emacs.d/rvf-captures.el")

(setq diary-file "~/Dropbox/emacs/diary")
(setq calendar-latitude [20 7 north])
(setq calendar-longitude [98 43 west])
(setq calendar-location-name "Pachuca, Hidalgo, Mexico")

;; see http://thread.gmane.org/gmane.emacs.gnus.general/82879/focus=83006
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
   (dired "~/Dropbox/paginas/discretas/source/org/")
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
;; (global-set-key
;;  (kbd "<f12> s")
;;  (lambda nil (interactive)
;;    (find-file "~/Dropbox/emacs/site-lisp/scratch.el")
;;    ))
(global-set-key
 (kbd "<f12> s")
 (lambda nil (interactive)
   (dired "~/Dropbox/paginas/sistemas-dinamicos/source/org/")
   ))
(global-set-key
 (kbd "<f12> t")
 (lambda nil (interactive)
   (dired "~/Dropbox/paginas/grad-topology/source/org/")
   ))

(global-set-key
 (kbd "<f12> v")
 (lambda nil (interactive)
   (dired "~/Dropbox/paginas/variable/source/org/")
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

;; ltx-help

(load "/usr/local/texlive/2016/texmf-dist/doc/latex/latex2e-help-texinfo/ltx-help.el")
(push "/usr/local/texlive/2016/texmf-dist/doc/info/" Info-directory-list)

(define-key help-map "\C-l" 'latex-help)

;; taken from http://www.emacswiki.org/emacs/AUCTeX
(defun latex-help-get-cmd-alist () ;corrected version:
  "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
    ;; mm, does it contain any cached entries
    (if (not (assoc "\\begin" latex-help-cmd-alist))
        (save-window-excursion
  	(setq latex-help-cmd-alist nil)
  	(Info-goto-node (concat latex-help-file "Command Index"))
          (end-of-buffer)
          (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
            (setq key (ltxh-buffer-substring (match-beginning 1) (match-end 1)))
            (setq value (ltxh-buffer-substring (match-beginning 2) (match-end 2)))
            (setq latex-help-cmd-alist
                  (cons (cons key value) latex-help-cmd-alist))))
      )
    latex-help-cmd-alist
    )
