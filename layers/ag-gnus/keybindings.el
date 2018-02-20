;;; keybindings.el --- ag-gnus layer keybindings
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

(with-eval-after-load 'gnus
  (unbind-key (kbd "\ek") gnus-group-mode-map)
  (unbind-key (kbd "\ek") gnus-summary-mode-map))

;;; keybindings.el ends here
