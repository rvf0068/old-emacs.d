;; http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html
(defun djcb-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"
  (interactive)
  (when sound (shell-command
	       (concat "mplayer -really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
      (shell-command (concat "notify-send "
			     (if icon (concat "-i " icon) "")
			     " '" title "' '" msg "'"))
    ;; text only version
    (message (concat title ": " msg))))

;; the appointment notification facility
(setq
 appt-message-warning-time 15 ;; warn 15 min in advance
 appt-display-mode-line t     ;; show in the modeline
 appt-display-format 'window) ;; use our func

(appt-activate 1)              ;; active appt (appointment notification)

;; update appt each time agenda opened

(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

;; our little facade-function for djcb-popup
(defun djcb-appt-display (min-to-app new-time msg)
  (djcb-popup (format "Appointment in %s minute(s)" min-to-app) msg 
              "/usr/share/icons/gnome/48x48/status/appointment-soon.png"
              "/usr/share/sounds/ubuntu/stereo/phone-incoming-call.ogg"))

(setq appt-disp-window-function (function djcb-appt-display))
