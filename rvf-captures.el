(defun rvf/org-capture-protocol (keys description file)
  "Using protocols. Tags"
  `(,keys ,description entry
          (file ,file)
"* %c %^g\n   %U\n%?" :empty-lines 0))

(defun rvf/org-capture-protocol-tags-immediate (keys description file)
  "Using protocols, immediate, tags"
  `(,keys ,description entry
          (file ,file)
"* %c %^g\n   %U\n%?" :immediate-finish t :empty-lines 0))

(defun rvf/org-capture-misc (keys description file)
  "Misc capture, asking for title."
  `(,keys ,description entry
          (file ,file)
"* %^{Title}\n%u\n\n%?" :empty-lines 0))

(defun rvf/org-capture-todo (keys description file)
  "Misc capture, asking for title."
  `(,keys ,description entry
          (file+headline ,file "Tasks")
"* TODO %^{Task}\n%u\n\n%?" :empty-lines 0))

(defun rvf/org-capture-template-project (project key filename)
  "Generic Capture"
  `((,key ,project)
    (,(concat key "g") ,(concat project " in gnus") entry
     (file ,(concat "~/Dropbox/org/" filename))
     "* %:subject\n\n%U\n%a\n%:group\n\n%i" :empty-lines 0)
    (,(concat key "w") ,(concat project " in web") entry
     (file ,(concat "~/Dropbox/org/" filename))
     "* %c\n   %U\n%?" :immediate-finish t :empty-lines 0)
    (,(concat key "z") ,(concat project " misc") entry
     (file ,(concat "~/Dropbox/org/" filename))
     "* %^{Title}\n%u\n\n%?" :empty-lines 0)
  ))

(setq org-capture-templates
      `(
        ("a" "Email" entry
         (file "~/Dropbox/org/misc.org") 
         "* TODO Respond to [[%l][%:fromname]] %^g:email\n\n%?")
        ,(rvf/org-capture-protocol-tags-immediate
          "b" "Bookmark" "~/Dropbox/org/bookmarks.org")
        ,(rvf/org-capture-protocol-tags-immediate
          "d" "Downloads" "~/Dropbox/org/downloads.org")
        ,@(rvf/org-capture-template-project "Emacs" "e" "emacs.org")        
        ,@(rvf/org-capture-template-project "LaTeX" "l" "latex.org")        
        ,@(rvf/org-capture-template-project "Math" "m" "mathnotes.org")        
        ,@(rvf/org-capture-template-project "Org Mode" "o" "orgmodec.org")
        ,(rvf/org-capture-protocol
          "p" "Papers" "~/Dropbox/org/papers.org")       
        ,(rvf/org-capture-todo
          "t" "Todo" "~/Dropbox/org/misctodo.org")     
        ,@(rvf/org-capture-template-project "Ubuntu" "u" "ubuntu.org")
        ,(rvf/org-capture-misc
          "v" "Misc" "~/Dropbox/org/misc.org")  
        ,(rvf/org-capture-protocol
          "z" "Misc from web" "~/Dropbox/org/misc.org")        
      ))
