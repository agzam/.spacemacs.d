;;; keybindings.el --- ag-mail layer keybindings
;;
;; Copyright (c) 2017 Ag Ibragimov
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Code:

(with-eval-after-load 'mu4e
  (dolist (m '(mu4e-headers-mode-map mu4e-view-mode-map))
    (define-key m (kbd "C-=") 'mu4e-headers-split-view-grow)
    (define-key m (kbd "C--") 'mu4e-headers-split-view-shrink)))

;;; keybindings.el ends here
