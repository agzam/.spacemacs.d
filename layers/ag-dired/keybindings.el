;;; keybindings.el --- ag-dired layer keybindings.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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
  (evil-define-key 'insert eshell-mode-map (kbd "C-n") #'eshell-next-maching-input-from-input)

  ;; This has been bugging me - `<escape> k' gets translated into `M-k' which
  ;; for whatever reason forces direx to jump all the way to the bottom of the tree
  (define-key direx:direx-mode-map (kbd "M-k") #'direx:previous-item)
  (define-key direx:direx-mode-map (kbd "ESC k") #'direx:previous-item))

(add-hook 'eshell-mode-hook #'eshell-keybindings-override t)

;;; keybindings.el ends here
