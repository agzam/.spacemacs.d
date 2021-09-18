;;; keybindings.el --- ag-xwidget layer keybindings
;;
;; Copyright (c) 2021 Ag Ibragimov
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Code:

(spacemacs/set-leader-keys
  "jx" #'xwidget-webkit-url-get-create
  )

(spacemacs/set-leader-keys-for-major-mode
  'xwidget-webkit-mode
  "x" #'kill-xwidget-buffer

  )

;;; keybindings.el ends here
