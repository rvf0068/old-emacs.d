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
