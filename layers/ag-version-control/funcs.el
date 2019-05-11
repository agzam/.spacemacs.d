;;; funcs.el --- ag-version-control layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun get-branch-at-point ()
  (interactive)
  (let ((b (magit-branch-at-point)))
    (kill-new b)
    (message b)))

;;; funcs.el ends here
