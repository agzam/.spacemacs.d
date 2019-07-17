;;; funcs.el --- ag-mail layer functions
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

(defun mu4e-prepare-view ()
  (when (and
         mu4e-centered-view-p
         (spacemacs/toggle-fullscreen-frame-status))
    (spacemacs/toggle-centered-buffer)))

;;; funcs.el ends here
