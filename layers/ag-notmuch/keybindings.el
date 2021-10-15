;;; keybindings.el --- ag-notmuch layer keybindings
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

(spacemacs/set-leader-keys-for-major-mode 'notmuch-tree-mode
  "d" #'notmuch-tree-toggle-delete)

;;; keybindings.el ends here
