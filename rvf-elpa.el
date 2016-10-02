;; All this code was taken from Emacs Prelude!
;; https://github.com/bbatsov/prelude

(require 'package)
(require 'cl)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; set package-user-dir to be relative to Prelude install path

(package-initialize)

;; install a dummy org package
(when (not (package-installed-p 'org '(30000101)))
      (package-install-file "~/.emacs.d/dummy-org.el")
)

;; concurrent, deferred, used for anything-books
(defvar prelude-packages
  '(
    bbdb
    bibretrieve
    bibslurp
    calfw
    cdlatex
    chess
    concurrent
    dash
    deferred
    ess
    gap-mode
    google-this
    helm
    helm-bibtex
    helm-org-rifle
    magit
    markdown-mode
    move-text
    org-journal
    org-pomodoro
    org-ref
    org2blog
    orgbox
    pandoc-mode
    pdf-tools
    smart-mode-line
    smartparens
    smex
    xbm-life
    yasnippet
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

