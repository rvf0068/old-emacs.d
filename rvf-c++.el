;; from http://stackoverflow.com/a/27187158/577007

(require 'compile)

(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "g++ -Wall -g %s -o %s" (file-name-nondirectory buffer-file-name)
			 (file-name-base buffer-file-name)
			 ))
	    ))
