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
                            :repo "ch11ng/xelb"
                            ;; :commit "fe1b643e98ea4a87a3eed41b0bbaf6c12dfcfbec"
                            )
          :step pre)
    (exwm :location (recipe :fetcher github
                            :repo "ch11ng/exwm"
                            ;; :commit "b75c89cae2a1c4c70044f885c44a95fd2f9950dd"
                            )
          :step pre)
    helm-exwm
    pinentry
    desktop-environment
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
    (setq exwm-workspace-show-all-buffers nil)
    (setq exwm-layout-show-all-buffers t)

    :config
    ;; (spacemacs/exwm-bind-command "<s-return>"  exwm--terminal-command)

    (spacemacs/set-leader-keys
      "aG" 'exwm--switch-to-chrome
      "aS" 'exwm--switch-to-slack)

    (with-eval-after-load 'spaceline-segments
      (display-time-mode t)
      (spacemacs/toggle-mode-line-battery-on))

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

    (setq window-divider-default-right-width 2)
    (window-divider-mode)

    ;; allow hydras to capture all key presses (in line-mode, at least)
    (define-advice hydra-set-transient-map (:around (fun keymap on-exit &optional foreign-keys) exwm-passthrough)
      (setq exwm-input-line-mode-passthrough t)
      (let ((on-exit (lexical-let ((on-exit on-exit))
                       (lambda ()
                         (setq exwm-input-line-mode-passthrough nil)
                         (when on-exit (funcall on-exit))))))
        (funcall fun keymap on-exit foreign-keys)))

    (spacemacs|define-custom-layout "@google-chrome"
      :binding "g"
      :body (exwm--switch-to-chrome))

    (spacemacs|define-custom-layout "@slack"
      :binding "s"
      :body (exwm--switch-to-slack))

    ;;;; strange bug that sometimes loses keyboard in exwm app buffer after switching to it
    ;; (define-advice exwm-layout--set-client-list-stacking (:around (old-function) exwm-grab-kbd-after-set-client)
    ;;   "grab keyboard after exwm-layout--set-client-list-stacking"
    ;;   (if (derived-mode-p 'exwm-mode)
    ;;       (progn
    ;;         (exwm-input-grab-keyboard)
    ;;         (funcall old-function)
    ;;         )))

    ;; `exwm-input-set-key' allows you to set a global key binding (available in
    ;; any case). Following are a few examples.
    ;; + We always need a way to go back to line-mode from char-mode
    (delete ?\C-r exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "s-R") #'spacemacs/exwm-app-launcher)

    ;; exwm-buffer-transient-state
    (exwm-input-set-key (kbd "C-C z") (lambda ()
                                        (interactive)
                                        (when (derived-mode-p 'exwm-mode)
                                          (spacemacs/exwm-buffer-transient-state/body))))
    (delete ?\s-f exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "s-f") #'spacemacs/exwm-layout-toggle-fullscreen)
    (push ?\s-\[ exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "s-[") #'spacemacs/persp-go-prev)
    (push ?\s-\] exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "s-]") #'spacemacs/persp-go-next)
    (push ?\s-\C-\[ exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "s-C-[") #'spacemacs/exwm-workspace-prev)
    (push ?\s-\C-\] exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "s-C-]") #'spacemacs/exwm-workspace-next)
    (push ?\s-{ exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "s-{") #'spacemacs/exwm-workspace-prev)
    (push ?\s-} exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "s-}") #'spacemacs/exwm-workspace-next)
    (push ?\s-\` exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "s-`") #'exwm--app-next-window)

    (exwm-input-set-key (kbd "<C-s-escape>") (lambda () (interactive) (start-process "" nil exwm--suspend-command)))

    (require 'exwm-randr)
    (exwm-randr-enable)

    ;; switching monitors on and off
    (exwm-input-set-key (kbd "C-s-1") (lambda () (interactive) (start-process-shell-command "" nil "xrandr --output eDP1 --auto")))
    (exwm-input-set-key (kbd "C-s-2")
                        (lambda ()
                          (interactive)
                          (start-process-shell-command "" nil "xset r rate 200 60")
                          (start-process-shell-command "" nil "xrandr --output eDP1 --off --output DP1 --mode 2560x1440")))
    (exwm-input-set-key (kbd "C-s-3")
                        (lambda ()
                          (interactive)
                          (setq exwm-workspace-number 2)
                          (start-process-shell-command "" nil "~/.screenlayout/home-double-monitor.sh")
                          ;; (start-process-shell-command "" nil "killall yabar & ~/.screenlayout/home-double-monitor.sh && yabar")
                          ))

    (setq exwm-randr-workspace-output-plist '(1 "DP1" 2 "eDP1"))

    ;; (defun exwm--reset-bar ()
    ;;   (start-process-shell-command "yabar" nil "killall yabar && yabar &> /dev/null")
    ;;   ;; (start-process-shell-command
    ;;   ;;  "xrandr" nil "xrandr --output DP1 --mode 2560x1440 --right-of eDP1")
    ;;   )

    ;; (add-hook 'exwm-randr-screen-change-hook #'exwm--reset-bar)

    ;; The following example demonstrates how to set a key binding only available
    ;; in line mode. It's simply done by first push the prefix key to
    ;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
    ;; The example shorten 'C-c q' to 'C-q'.
    (push ?\C-q exwm-input-prefix-keys)
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
    ;; cmd+space - Spacemacs leader
    (push ?\s-\  exwm-input-prefix-keys)
    ;; universal Get-me-outta-here
    (push ?\C-g exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "C-g") #'keyboard-quit)
    ;; universal Arguments
    (push ?\C-u exwm-input-prefix-keys)
    (exwm-input-set-key (kbd "C-u") #'universal-argument)
    ;; C-c, C-x are needed for copying and pasting
    (delete ?\C-x exwm-input-prefix-keys)
    (delete ?\C-c exwm-input-prefix-keys)
    ;; We can use `M-m h' to access help
    (delete ?\C-h exwm-input-prefix-keys)

    (exwm-input-set-key (kbd "M-x") #'helm-M-x)
    (exwm-input-set-key (kbd "M-:") #'eval-expression)

    ;; layouts can be switched by S-return S-return
    (exwm-input-set-key (kbd "<s-return>") #'spacemacs/layouts-transient-state/body)
    (spacemacs/transient-state-register-add-bindings 'layouts
      '(("<s-return>" spacemacs/persp-perspectives :exit t)))

    (exwm-input-set-key (kbd "s-u") #'winner-undo)
    (exwm-input-set-key (kbd "S-s-U") #'winner-redo)
    (exwm-input-set-key (kbd "C-c C-r") #'exwm-reset)
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
            ([?\s-j] . [\C-tab])
            ([?\s-k] . [\C-S-tab])
            ([?\s-w] . [\C-f4])
            ([?\s-t] . [\C-t])
            ([?\s-r] . [\C-r])
            ([?\s-a] . [\C-a])
            ([?\s-x] . [\C-x])
            ([?\s-c] . [\C-c])
            ([?\s-v] . [\C-v])))

    (exwm-input-set-simulation-keys exwm-input-simulation-keys)

    (defun exwm--set-local-simulation-keys ()
      (when (derived-mode-p 'exwm-mode)
        (let ((local-keys (append
                           exwm-input-simulation-keys
                           (pcase exwm-class-name
                             ("Slack" '(([?\C-i] . [\M-right])
                                        ([?\C-o] . [\M-left])))
                             ("Google-chrome" '(([?\s-w] . [\C-w])))))))
          (exwm-input-set-local-simulation-keys local-keys))))

    (add-hook 'window-configuration-change-hook 'exwm--set-local-simulation-keys)

    (exwm-input-set-key (kbd "M-y") #'helm-show-kill-ring)

    (define-advice helm-kill-ring-action-yank (:around (old-function str) exwm-paste)
      "Paste the selection appropriately in exwm mode buffers"
      (if (derived-mode-p 'exwm-mode)
          (progn
            (kill-new str)
            (exwm-reset)
            (exwm-input--fake-key ?\C-v))
        (funcall old-function str)))

    (add-hook #'kill-buffer-hook #'fix-exwm-kill-buffer)))

(defun ag-exwm/init-helm-exwm ()
  (use-package helm-exwm
    :config
    (exwm-input-set-key (kbd "M-s-b") #'helm-exwm)))

(defun ag-exwm/init-pinentry ()
  (use-package pinentry
    :config
    (setq epa-pinentry-mode 'loopback)
    (pinentry-start)))

(defun ag-exwm/init-exwm-edit ()
  (use-package exwm-edit
    :demand t
    :config
    (defun ag-exwm/on-exwm-edit-compose ()
      (spacemacs/toggle-visual-line-navigation-on)
      (funcall 'markdown-mode))
    (add-hook 'exwm-edit-compose-hook 'ag-exwm/on-exwm-edit-compose)))

(defun ag-exwm/init-desktop-environment ()
  (use-package desktop-environment
    :config
    (spacemacs/set-leader-keys
      "M" #'spacemacs/desktop-environment-transient-state/body)))

;;; packages.el ends here
