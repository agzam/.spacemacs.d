;;; funcs.el --- ag-exwm layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; funcs.el ends here

(defun spacemacs/exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))

(defun spacemacs/exwm-app-launcher (command)
  "Launche an application in your PATH.
Can show completions at point for COMMAND using helm or ido"
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (start-process-shell-command command nil command))

(defun spacemacs/exwm-layout-toggle-fullscreen ()
  "Togggle full screen for Emacs and X windows"
  (interactive)
  (if exwm--id
      (if (exwm-layout--fullscreen-p)
          (exwm-reset)
        (exwm-layout-set-fullscreen))
    (spacemacs/toggle-maximize-buffer)))

(defun exwm--find-app-buffer (class-name)
  "Return exwm buffer with given `CLASS-NAME', nil if none found"
  (cdr
   (first
    (-filter
     (lambda (x)
       (equal (buffer-local-value 'exwm-class-name (cdr x))
              class-name))
     exwm--id-buffer-alist))))

(defun exwm--app-next-window (&optional show-list)
  "Jump to next window of current exwm app (if there is more than one).

With universal argument shows list of windows"
  (interactive "P")
  (let* ((cur-b (current-buffer))
         (class (buffer-local-value 'exwm-class-name cur-b))
         (bs* (-map #'cdr
                    (-filter
                     (lambda (x) (equal (buffer-local-value 'exwm-class-name (cdr x)) class))
                     exwm--id-buffer-alist)))
         (bs (-take-while (lambda (x)
                            (not (equal cur-b x))) bs*)))
    (if show-list
      (helm-exwm (function
                  (lambda ()
                    (and (equal class exwm-class-name)
                         (not (equal cur-b (current-buffer)))))))
      (if bs
          (switch-to-buffer (car (last bs)))
        (switch-to-buffer (car (last bs*)))))))

(defun exwm--switch-to-chrome ()
  (interactive)
  (if-let ((buf (exwm--find-app-buffer "Google-chrome")))
      (switch-to-buffer buf)
    (spacemacs/exwm-app-launcher "google-chrome-stable")))

(defun exwm--switch-to-slack ()
  (interactive)
  (if-let ((buf (exwm--find-app-buffer "Slack")))
      (switch-to-buffer buf)
    (spacemacs/exwm-app-launcher "slack")))

(defun exwm-zoom (zoom-type)
  "Send C-= or C-- or C-0 to exwm app - depending on ZOOM-TYPE

ZOOM-TYPE can be 'in 'out or 'reset"
  (cond ((eq zoom-type 'in) (exwm-input--fake-key ?\C-=))
        ((eq zoom-type 'out) (exwm-input--fake-key ?\C--))
        ((eq zoom-type 'reset) (exwm-input--fake-key ?\C-0))))

(spacemacs|define-transient-state exwm-buffer
  :title "EXWM app buffer transient mode"
  :doc "
 Zoom^^^^^^              Window^^^^^              Other^^
 ────^^^^^^───────────── ──────^^^^^───────────── ─────^^──────────
 [_+_/_=_/_j_] in        [_h_] left              [_f_]^^^ fullscreen
 [_-_/_k_]^^   out       [_l_] right             [_F_]^^^ floating
 [_0_]^^^^   reset       [_m_] maximize          [_d_] kill app
      ^^^^               ^^^^                    [_q_]^^^ quit
"
  :bindings
  ("+" (exwm-zoom 'in))
  ("=" (exwm-zoom 'in))
  ("j" (exwm-zoom 'in))
  ("-" (exwm-zoom 'out))
  ("k" (exwm-zoom 'out))
  ("0" (exwm-zoom 'reset) :exit t)
  ("f" #'spacemacs/exwm-layout-toggle-fullscreen :exit t)
  ("h" #'evil-window-left)
  ("l" #'evil-window-right)
  ("F" #'exwm-floating-toggle-floating)
  ("m" #'spacemacs/toggle-maximize-buffer)
  ("d" #'spacemacs/kill-this-buffer :exit t)
  ("q" nil :exit t))

(advice-add #'exwm-floating-toggle-floating :before #'delete-other-windows)

(defun lock-screen ()
  (interactive)
  (start-process-shell-command "" nil "xscreensaver-command -lock"))

(defun xdotool-key (key)
  (interactive)
  (start-process-shell-command "" nil (concat "xdotool key " key)))

(defun switch-to-music-player-app ()
  (interactive)
  (start-process-shell-command "" nil "/usr/bin/gpmdp"))

(defun spacemacs/exwm-workspace-next ()
  "Switch to next exwm-workspaceective (to the right)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index
                       (1- exwm-workspace-number))))
    (cond
     (only-workspace? nil)
     (overflow?
      (exwm-workspace-switch 0))
     (t (exwm-workspace-switch  (+ 1 exwm-workspace-current-index))))))

(defun spacemacs/exwm-workspace-prev ()
  "Switch to next exwm-workspaceective (to the right)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index 0)))
    (cond
     (only-workspace? nil)
     (overflow? (exwm-workspace-switch (1- exwm-workspace-number)))
     (t (exwm-workspace-switch  (1- exwm-workspace-current-index))))))

(spacemacs|define-transient-state desktop-environment
  :title "dektop-environment transient group"
  :doc "
  volume       music         mute       other
 ─────────── ───────────── ────────── ────────────────
 [_j_] lower   [_p_] pause      ^^[_m_] mute  [_s_] screenshot
 [_k_] higher  [_h_] prev.      ^^[_M_] mic   [_S_] part. screenshot
 ^^            [_l_] next       ^^^^          [_L_] lock screen
 ^^            [_a_] music app
"
  :bindings
  ("j" (xdotool-key "XF86AudioLowerVolume"))
  ("k" (xdotool-key "XF86AudioRaiseVolume"))
  ("p" (xdotool-key "XF86AudioPlay"))
  ("h" (xdotool-key "XF86AudioPrev"))
  ("l" (xdotool-key "XF86AudioNext"))
  ("a" nil)
  ("m" (xdotool-key "XF86AudioMute") :exit t)
  ("M" #'desktop-environment-toggle-microphone-mute)
  ("s" #'desktop-environment-screenshot :exit t)
  ("S" #'desktop-environment-screenshot-part :exit t)
  ("L" #'lock-screen :exit t)
  ("q" nil :exit t))
