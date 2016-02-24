;; C++
;; from http://stackoverflow.com/a/27187158/577007

(require 'compile)

(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "g++ -Wall -g %s -o %s" (file-name-nondirectory buffer-file-name)
			 (file-name-base buffer-file-name)
			 ))
	    (smartparens-mode 1)
	    (yas/minor-mode-on)
	    )
	  )

;; Python
;; from http://stackoverflow.com/a/33436265/577007
(add-to-list 'auto-mode-alist '("\\.sage\\'" . python-mode))

(add-hook 'python-mode-hook
          (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "python " buffer-file-name)
		 )
	    (smartparens-mode 1)
	    (yas/minor-mode-on)
	    )
	  )

;; Perl

(add-hook 'perl-mode-hook
          (lambda ()
	    (set (make-local-variable 'compile-command)
		 (concat "perl " buffer-file-name)
		 )
	    (smartparens-mode 1)
	    (yas/minor-mode-on)
	    )
	  )

;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (smartparens-mode 1)
	    (yas/minor-mode-on)
	    ))

(defun xml-electric-dollar nil
  "When called once, insert <M></M> and leave point in between.
When called twice, replace the previously inserted stuff by one $."
  (interactive)
  (if (and (looking-at "</M>") (looking-back "<M>"))
      (progn (delete-char 4)
             (delete-char -3)
             (insert "$"))
    (insert "<M></M>")
    (backward-char 4)))

;; XML

(add-hook 'nxml-mode-hook
	  (lambda ()
	    (smartparens-mode 1)
	    (yas/minor-mode-on)
	    (local-set-key (kbd "$") 'xml-electric-dollar)
	    ))

;; GAP

(defun my/insert-tag (tag-name)
  (if (region-active-p) (goto-char (region-beginning)))
  (insert (concat "<" tag-name ">"))
  (if (region-active-p) (goto-char (region-end)))
  (insert (concat "</" tag-name ">"))
  (if (region-active-p) ()
    (backward-char (+ (length tag-name) 3)))
)

(defun my/insert-env (tag-name)
  (if (region-active-p) (progn
			 (goto-char (region-beginning))
			 (forward-line -1)
			 (indent-according-to-mode)
			 ))
  (insert (concat "<" tag-name ">"))
  (if (region-active-p) (progn
			 (goto-char (region-beginning))
			 (forward-line 1)
			 (indent-according-to-mode)
			 ))
  (insert (concat "</" tag-name ">"))
  (if (region-active-p) ()
    (backward-char (+ (length tag-name) 3)))
)

(add-hook 'gap-mode-hook
	  (lambda()
	    (smartparens-mode 1)
	    (yas/minor-mode-on)
	    (local-set-key (kbd "$") 'xml-electric-dollar)
	    (local-set-key
	     (kbd "<")
      (defhydra hydra-xml-gap (:hint nil)
        "
        Tags  _a_ Argument
              _e_ Example
              _t_ Title
              _<_ < 
        "
        ("a" (my/insert-tag "A"))
        ("e" (my/insert-env "Example"))
        ("t" (my/insert-tag "Title"))
        ("<" (insert "<"))
	))
	  ))

