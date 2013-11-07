;; From: sandipchitale@yahoo.com (Sandip Chitale)
;; Newsgroups: gnu.emacs.sources
;; Subject: CODE: Duplicate line or region backward or forward
;; Date: 7 Feb 2004 12:15:35 -0800
(defun duplicate-line-backward ()
  "Duplicate the current line backward."
  (interactive "*")
  (save-excursion
    (let ((contents (buffer-substring
                     (line-beginning-position)
                     (line-end-position))))
      (beginning-of-line)
      (insert contents ?\n)))
  (previous-line 1))

(defun duplicate-line-forward ()
  "Duplicate the current line forward."
  (interactive "*")
  (save-excursion
    (let ((contents (buffer-substring
                     (line-beginning-position)
                     (line-end-position))))
      (end-of-line)
      (insert ?\n contents)))
  (next-line 1))

;; from
;; http://stackoverflow.com/questions/6845005/how-can-i-open-files-externally-in-emacs-dired-mode
(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)))
