(require 'dired)
(require 'dired-x)
(require 'dired-aux)

;;; from http://stackoverflow.com/questions/23029329/how-can-emacs-make-a-open-with-selectable-for-files
(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "evince" "acroread")
        ("\\.eps\\'" "evince")
        ("\\.djvu\\'" "evince")
        ("\\.jpe?g\\'" "eog" "shotwell")
        ("\\.png\\'" "eog" "shotwell")
        ("\\.gif\\'" "eog")
        ("\\.xpm\\'" "eog")
        ("\\.csv\\'" "libreoffice")
        ("\\.tex\\'" "pdflatex" "latex")
        ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\'" "smplayer")
        ("\\.\\(?:mp3\\|flac\\)\\'" "audacious")
        ("\\.html?\\'" "firefox")
        ("\\.cue\\'" "audacious")
        ("\\.epub\\'" "fbreader")
	))

(defun dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      files)))
  (apply
   #'start-process
   (list cmd nil shell-file-name shell-command-switch
         (format "nohup 1>/dev/null 2>/dev/null %s \"%s\""
                 (if (> (length file-list) 1)
                     (format "%s %s"
                             cmd
                             (cadr (assoc cmd dired-filelist-cmd)))
                   cmd)
                 (mapconcat #'expand-file-name file-list "\" \"")))))

(define-key dired-mode-map (kbd "r") 'dired-start-process)

