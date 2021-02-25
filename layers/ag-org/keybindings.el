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
(global-set-key (kbd "C-c C-f") #'org-roam-find-file)
(global-set-key (kbd "C-c r") #'org-roam-dailies-capture-date)
(org-defkey org-mode-map (kbd "C-c C-f") #'org-roam-find-file)

(evil-define-key 'normal org-mode-map "H" 'org-shiftleft)
(evil-define-key 'normal org-mode-map "L" 'org-shiftright)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "sa" 'org-toggle-archive-tag
  "sA" 'org-archive-subtree
  "sx" 'org-cut-subtree
  "gl" 'org-goto-last-heading
  "gd" 'org-goto-datetree-date)

;;; keybindings.el ends here
