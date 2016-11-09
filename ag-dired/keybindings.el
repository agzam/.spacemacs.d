(spacemacs/set-leader-keys
  "ft" 'direx:jump-to-directory
  "pt" 'direx-project:jump-to-project-root)

(evil-define-key 'normal direx:direx-mode-map "r" 'direx:refresh-whole-tree)
(evil-define-key 'normal dired-mode-map "o" 'spacemacs/dired-open-item-other-window-transient-state/body)

