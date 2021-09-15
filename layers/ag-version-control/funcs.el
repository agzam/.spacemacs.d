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

(defun magit-log-orig_head--head (args files)
  "Compare log since the last pull. i.e.: show only commits between last pull and head"
  (interactive (magit-log-arguments))
  (magit-log-other
   (list "ORIG_HEAD..HEAD")
   (car (magit-log-arguments)) files))

(defun magit-log-other--current (revision)
  "Compare log between branches à la GitHub style.
i.e.: show only commits that differ between selected (other branch) and current branch"
  (interactive (list (magit-read-other-branch-or-commit "Log compare")))
  (magit-log-other
   (list (concat revision ".." (magit-get-current-branch)))
   (car (magit-log-arguments)) nil))

(defun magit-log--origin-master ()
  "Compare log between branches à la GitHub style between current branch and origin/master"
  (interactive)
  (magit-log-other
   (list (concat  "origin/master.." (magit-get-current-branch)))
   (car (magit-log-arguments)) nil))

(defun magit-diff-range-reversed (rev-or-range &optional args files)
  "Diff between two branches. Unlike `diff-range` works in opposite order i.e.: `base..current`"
  (interactive (list (magit-read-other-branch-or-commit "Diff range")))
  (magit-diff-range (concat rev-or-range ".." (magit-get-current-branch)) args files))

(defun magit-diff--origin-master ()
  "Compare log between branches à la GitHub style between current branch and origin/master"
  (interactive)
  (magit-diff-range (concat "origin/master.." (magit-get-current-branch))))

;;; funcs.el ends here
