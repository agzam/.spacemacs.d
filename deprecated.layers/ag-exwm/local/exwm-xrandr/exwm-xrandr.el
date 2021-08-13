;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; exwm-xrandr.el --- exwm-xrandr

;;; Commentary:
;;

(require 'exwm-randr)
;;; Code:

(exwm-randr-enable)

(defvar exwm--xrandr-mods
  '((laptop . "xrandr --output VIRTUAL1 --off --output eDP1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP1 --off --output HDMI2 --off --output HDMI1 --off --output DP2 --off")
   (external-home . "xrandr --output eDP1 --off --output DP2 --primary --mode 2560x1440")
   (laptop+external-home . "xrandr --output eDP1 --mode 1920x1080 --pos 0x0 --rotate normal --output DP1 --off --output DP2 --primary --mode 2560x1440 --pos 1920x0 --rotate normal --output HDMI1 --off --output HDMI2 --off --output VIRTUAL1 --off"))
  "Describes different xrandr modes. One can add values by tweaking display setup in 'arandr' app and saving it into a file, then using those settings here")

(defun exwm-xrandr--set-keybindings ()
  "For every value in `exwm--xrandr-mods', add a keybiding. e.g. `(kbd \"C-s-2\")' activates second mode in the list"
  (dotimes (i (length exwm--xrandr-mods))
    (exwm-input-set-key
     (kbd (concat "C-s-" (int-to-string (+ i 1))))
     (lambda ()
       (interactive)
       (start-process-shell-command "" nil "xset r rate 200 60")
       (start-process-shell-command
        "" nil (cdr (nth i exwm--xrandr-mods)))))))


(provide 'exwm-xrandr)

;;; exwm-xrandr.el ends here
