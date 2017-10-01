(spacemacs/set-leader-keys
  "ft" 'direx:jump-to-directory
  "pt" 'direx-project:jump-to-project-root)

(evil-define-key 'normal direx:direx-mode-map "r" 'direx:refresh-whole-tree)
(evil-define-key 'normal dired-mode-map "o" 'spacemacs/dired-open-item-other-window-transient-state/body)

(defun eshell-keybindings-override ()
  (define-key eshell-mode-map (kbd "C-l")
    (lambda ()
      (interactive)
      (eshell/clear-scrollback)
      (eshell-reset)))
  (evil-define-key 'normal eshell-mode-map (kbd "RET") #'eshell-action-on-file-or-dir-at-point)
  (evil-define-key 'normal eshell-mode-map (kbd "o") #'eshell-action-on-file-or-dir-at-point-other-window)
  (evil-define-key 'insert eshell-mode-map (kbd "C-p") #'eshell-previous-matching-input-from-input)
  (evil-define-key 'insert eshell-mode-map (kbd "C-n") #'eshell-next-matching-input-from-input))

(add-hook 'eshell-mode-hook #'eshell-keybindings-override t)
