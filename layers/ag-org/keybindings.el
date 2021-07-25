;;; keybindings.el --- ag-org layer keybindings
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(global-set-key (kbd "C-x C-p") 'org-pomodoro)
(global-set-key (kbd "C-x p") 'org-pomodoro)
(global-set-key (kbd "C-c C-f") #'org-roam-node-find)
(global-set-key (kbd "C-c r") #'org-roam-dailies-capture-date)
(org-defkey org-mode-map (kbd "C-c C-f") #'org-roam-node-find)

(evil-define-key 'normal org-mode-map "H" #'org-shiftleft)
(evil-define-key 'normal org-mode-map "L" #'org-shiftright)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "sa" #'org-toggle-archive-tag
  "sA" #'org-archive-subtree
  "sN" #'widen
  "sx" #'org-cut-subtree
  "gL" #'org-goto-last-heading
  "gd" #'org-goto-datetree-date
  "TL" #'org-toggle-link-display ; otherwise conflicts with lsp bindings
  "n" #'org-next-link
  "p" #'org-previous-link
  "ri" #'org-roam-node-insert
  "ir" #'org-roam-node-insert
  "rl" #'org-roam-buffer-toggle
  "rf" #'org-roam-node-find)

(spacemacs/set-leader-keys-for-major-mode 'org-roam-mode
  "rf" #'org-roam-node-find
  "rl" #'org-roam-buffer-toggle)

(evil-define-key 'insert org-mode-map (kbd "C-c i") #'org-roam-node-insert)

(spacemacs/set-leader-keys
  "aoL" #'org-store-link-id-optional)

;;; keybindings.el ends here
