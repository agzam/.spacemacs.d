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

;; don't override Evil's g and G in dired
(unbind-key "g" dired-mode-map)
(unbind-key "G" dired-mode-map)

(evil-define-key 'normal direx:direx-mode-map "r" 'direx:refresh-whole-tree)
(evil-define-key '(normal evilified) dired-mode-map
  "o" 'spacemacs/dired-open-item-other-window-transient-state/body
  "s" 'hydra-dired-quick-sort/body
  "gr" 'revert-buffer
  (kbd "RET") 'dired-find-file
  (kbd "<C-return>") 'dired-find-alternate-file) ; re-use dired buffer, insead of keeping it open

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)   ; re-use dired buffer, insead of keeping it open
(define-key dired-mode-map (kbd "<C-return>") 'dired-find-alternate-file)

;; This has been bugging me - `<escape> k' gets translated into `M-k' which
;; for whatever reason forces direx to jump all the way to the bottom of the tree
(unbind-key (kbd "M-k"))
(define-key direx:direx-mode-map (kbd "M-k") #'direx:previous-item)
(define-key direx:direx-mode-map (kbd "ESC k") #'direx:previous-item)

;;; keybindings.el ends here
