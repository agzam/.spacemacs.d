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

;; remove when https://github.com/sshaw/git-link/pull/70 gets merged
(defun git-link-blame ()
  (interactive)
  (cl-flet ((git-link--new* (x) (replace-regexp-in-string "/blob/" "/blame/" x)))
    (advice-add 'git-link--new :override #'git-link--new*)
    (let ((link (call-interactively 'git-link)))
      (advice-remove 'git-link--new #'git-link--new*)
      (git-link--new link))))

;;; funcs.el ends here
