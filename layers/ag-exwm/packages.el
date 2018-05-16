;;; packages.el --- ag-exwm layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-exwm-packages
  '(cl-generic
    (xelb :location (recipe :fetcher github
                            :repo "ch11ng/xelb")
          :step pre)
    (exwm :location (recipe :fetcher github
                            :repo "ch11ng/exwm")
          :step pre)
    helm-exwm
    pinentry
    gpastel
    (exwm-edit :location local)))

(defun ag-exwm/init-cl-generic ()
  (use-package cl-generic
    :demand))

(defun ag-exwm/init-xelb ()
  (use-package xelb))

(defun ag-exwm/init-exwm ()
  (use-package exwm
    :init
    (setq use-dialog-box nil) ;; Disable dialog boxes since they are unusable in EXWM
    (setq exwm-workspace-number 1)
    (setq winum-scope 'frame-local) ;; otherwise it jumps accros frames on a multi-monitor setup
    (setq persp-init-frame-behaviour nil) ;; otherwise spacemacs layouts would mess floating windows
    :config
    (defun spacemacs/exwm-bind-command (key command &rest bindings)
      (while key
        (exwm-input-set-key (kbd key)
                            `(lambda ()
                               (interactive)
                               (start-process-shell-command ,command nil ,command)))
        (setq key     (pop bindings)
              command (pop bindings))))

    ;; (spacemacs/exwm-bind-command "<s-return>"  exwm--terminal-command)

    ;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
    ;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
    ;; when a new window class name or title is available. Here's some advice on
    ;; this subject:
    ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
    ;; + Only renaming buffer in one hook and avoid it in the other. There's no
    ;;   guarantee on the order in which they are run.
    ;; + For applications with multiple windows (e.g. GIMP), the class names of all
    ;;   windows are probably the same. Using window titles for them makes more
    ;;   sense.
    ;; + Some application change its title frequently (e.g. browser, terminal).
    ;;   Its class name may be more suitable for such case.
    ;; In the following example, we use class names for all windows expect for
    ;; Java applications and GIMP.

    (add-hook 'exwm-update-class-hook
              (lambda ()
                (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                            (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-class-name))))
    (add-hook 'exwm-update-title-hook
              (lambda ()
                (when (or (not exwm-instance-name)
                          (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                          (string= "gimp" exwm-instance-name))
                  (exwm-workspace-rename-buffer exwm-title))))

    ;; strange bug that sometimes loses keyboard in exwm app buffer after switching to it
    ;; (define-advice exwm-layout--set-client-list-stacking (:around (old-function) exwm-grab-kbd-after-set-client)
    ;;   "grab keyboard after exwm-layout--set-client-list-stacking"
    ;;   (if (derived-mode-p 'exwm-mode)
    ;;       (progn
    ;;         (funcall old-function)
    ;;         (exwm-input-grab-keyboard))))

    (defun spacemacs/exwm-layout-toggle-fullscreen ()
      "Togggles full screen for Emacs and X windows"
      (interactive)
      (if exwm--id
          (if (exwm-layout--fullscreen-p)
              (exwm-reset)
            (exwm-layout-set-fullscreen))
        (spacemacs/toggle-maximize-buffer)))

    (defun spacemacs/exwm-app-launcher (command)
      "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ido"
      (interactive (list (read-shell-command exwm-app-launcher--prompt)))
      (start-process-shell-command command nil command))

    ;; `exwm-input-set-key' allows you to set a global key binding (available in
    ;; any case). Following are a few examples.
    ;; + We always need a way to go back to line-mode from char-mode
    ;; (exwm-input-set-key (kbd "s-r") 'exwm-reset)
    (delete ?\s-r exwm-input-prefix-keys)

    (exwm-input-set-key (kbd "s-f") #'spacemacs/exwm-layout-toggle-fullscreen)
    (exwm-input-set-key (kbd "s-r") #'spacemacs/exwm-app-launcher)

    (exwm-input-set-key (kbd "<C-s-escape>") (lambda () (interactive) (start-process "" nil exwm--suspend-command)))

    ;; switching monitors on and off
    (exwm-input-set-key (kbd "C-s-1") (lambda () (interactive)
                                          (start-process-shell-command "" nil "xrandr --output eDP-1 --auto")))
    (exwm-input-set-key (kbd "C-s-2") (lambda () (interactive) (start-process-shell-command "" nil "xrandr --output eDP-1 --off --output DP-1 --mode 2560x1440")))

    ;; The following example demonstrates how to set a key binding only available
    ;; in line mode. It's simply done by first push the prefix key to
    ;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
    ;; The example shorten 'C-c q' to 'C-q'.
    (push ?\C-q exwm-input-prefix-keys)
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
    ;; M-m leader, sorry Space Folks
    (push ?\s-\  exwm-input-prefix-keys)
    ;; Universal Get-me-outta-here
    (push ?\C-g exwm-input-prefix-keys)
    ;; Universal Arguments
    (push ?\C-u exwm-input-prefix-keys)
    ;; (push ?\C-0 exwm-input-prefix-keys)
    ;; (push ?\C-1 exwm-input-prefix-keys)
    ;; (push ?\C-2 exwm-input-prefix-keys)
    ;; (push ?\C-3 exwm-input-prefix-keys)
    ;; (push ?\C-4 exwm-input-prefix-keys)
    ;; (push ?\C-5 exwm-input-prefix-keys)
    ;; (push ?\C-6 exwm-input-prefix-keys)
    ;; (push ?\C-7 exwm-input-prefix-keys)
    ;; (push ?\C-8 exwm-input-prefix-keys)
    ;; (push ?\C-9 exwm-input-prefix-keys)
    ;; C-c, C-x are needed for copying and pasting
    (delete ?\C-x exwm-input-prefix-keys)
    (delete ?\C-c exwm-input-prefix-keys)
    ;; We can use `M-m h' to access help
    (delete ?\C-h exwm-input-prefix-keys)

    ;; Preserve the habit
    ;; (exwm-input-set-key (kbd "s-x") 'helm-M-x)
    ;; (exwm-input-set-key (kbd "s-;") 'evil-ex)
    ;; Shell (not a real one for the moment)
    (exwm-input-set-key (kbd "C-'") #'spacemacs/default-pop-shell)
    (exwm-input-set-key (kbd "<s-return>") #'spacemacs/layouts-transient-state/body)
    (exwm-input-set-key (kbd "s-u") #'winner-undo)
    (exwm-input-set-key (kbd "S-s-U") #'winner-redo)
    ;; Change buffers
    (exwm-input-set-key (kbd "s-b") #'spacemacs-layouts/non-restricted-buffer-list-helm)

    ;; (push [s-tab] exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "<s-tab>") #'previous-buffer)

    ;; Focusing windows
    ;; (exwm-input-set-key (kbd "s-h") #'evil-window-left)
    ;; (exwm-input-set-key (kbd "s-j") #'evil-window-down)
    ;; (exwm-input-set-key (kbd "s-k") #'evil-window-up)
    ;; (exwm-input-set-key (kbd "s-l") #'evil-window-right)
    ;; Moving Windows
    ;; (exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
    ;; (exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
    ;; (exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
    ;; (exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)
    ;; ;; Resize
    ;; (exwm-input-set-key (kbd "M-s-h") #'spacemacs/shrink-window-horizontally)
    ;; (exwm-input-set-key (kbd "M-s-j") #'spacemacs/shrink-window)
    ;; (exwm-input-set-key (kbd "M-s-k") #'spacemacs/enlarge-window)
    ;; (exwm-input-set-key (kbd "M-s-l") #'spacemacs/enlarge-window-horizontally)
    ;; Workspaces
    ;; (exwm-input-set-key (kbd "s-]") #'spacemacs/exwm-workspace-next)
    ;; (exwm-input-set-key (kbd "s-[") #'spacemacs/exwm-workspace-prev)
    (add-to-list 'undo-tree-incompatible-major-modes 'exwm-mode)
    (evil-set-initial-state 'exwm-mode 'emacs) ;; otherwise simulation keys won't work
    (setq exwm-input-simulation-keys
          '(([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-s] . [\C-f])
            ([?\M-h] . [left])
            ([?\M-l] . [right])
            ([?\M-j] . [down])
            ([?\M-k] . [up])
            ([?\s-l] . [\C-tab])
            ([?\s-h] . [\C-S-tab])
            ([?\s-w] . [\C-f4])
            ([?\s-t] . [\C-t])
            ([?\s-a] . [\C-a])
            ([?\s-x] . [\C-x])
            ([?\s-c] . [\C-c])
            ([?\s-v] . [\C-v])))

    (exwm-input-set-simulation-keys exwm-input-simulation-keys)

    ;; (evil-define-key 'normal 'exwm-mode-map (kbd "C-p") nil)
    ;; (evil-define-key 'normal 'exwm-mode-map (kbd "C-n") nil)
    ;; (setq exwm-input-line-mode-passthrough nil)

    (require 'exwm-randr)
    ;; (setq exwm-randr-workspace-output-plist '(1 "DP-1" 2 "eDP-1"))
    ;; (add-hook 'exwm-randr-screen-change-hook
    ;;           (lambda ()
    ;;             (start-process-shell-command
    ;;              "xrandr" nil "xrandr --output DP-1 --mode 2560x1440 --right-of eDP-1")))
    (exwm-randr-enable)

    (setq window-divider-default-right-width 1)
    (window-divider-mode)))

(defun ag-exwm/init-helm-exwm ()
  (use-package helm-exwm
    :config
    (exwm-input-set-key (kbd "M-s-b") #'helm-exwm)))

(defun ag-exwm/init-pinentry ()
  (use-package pinentry
    :config
    (setq epa-pinentry-mode 'loopback)
    (pinentry-start)))

(defun ag-exwm/init-gpastel ()
  (use-package gpastel
    :config
    (add-hook 'exwm-init-hook 'gpastel-start-listening)

    (exwm-input-set-key (kbd "M-y") #'helm-show-kill-ring)

    (define-advice helm-kill-ring-action-yank (:around (old-function str) exwm-paste)
      "Paste the selection appropriately in exwm mode buffers"
      (if (derived-mode-p 'exwm-mode)
          (progn
            (kill-new str)
            (exwm-reset)
            (exwm-input--fake-key ?\C-v))
        (funcall old-function str)))))

(defun ag-exwm/init-exwm-edit ()
  (use-package exwm-edit
    :demand t))

;;; packages.el ends here
