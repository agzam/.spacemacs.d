;;; packages.el --- ag-version-control layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-version-control-packages '(magit gist))

(setq
 vc-follow-symlinks t
 diff-hl-side 'left)

(defun ag-version-control/post-init-magit ()
  (setq
   ;; version-control-diff-tool 'diff-hl
   version-control-global-margin t
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   magit-repository-directories '("~/DevProjects" "~/Sandbox")
   magit-show-refs-arguments '("--sort=-committerdate")
   magit-delete-by-moving-to-trash nil
   magit-branch-rename-push-target nil ; do not push renamed/deleted branch to remote automatically
   magit-diff-refine-hunk 'all
   )

  (define-key transient-map "q" #'transient-quit-all)

  ;; TODO: re-write making things compatible with magit-transient
  ;; Add `git log ORIG_HEAD..HEAD` to magit
  ;; that lets you log only changes since the last pull
  (defun magit-log-orig_head--head (&optional args files)
    "Compare log since the last pull. i.e.: show only commits between last pull and head"
    (interactive (magit-log-arguments))
    (magit-log (list "ORIG_HEAD..HEAD") args files))

  (magit-define-popup-action 'magit-log-popup
    ?p "orig_head..head" 'magit-log-orig_head--head)

  (defun magit-log-other--current (revision)
    "Compare log between branches à la GitHub style.
i.e.: show only commits that differ between selected (other branch) and current branch"
    (interactive (list (magit-read-other-branch-or-commit "Log compare")))
    (magit-log (list (concat revision ".." (magit-get-current-branch)))))

  (magit-define-popup-action 'magit-log-popup
    ?R "other..current" 'magit-log-other--current)

  (defun magit-log--origin-master ()
    "Compare log between branches à la GitHub style between current branch and origin/master"
    (interactive)
    (magit-log (list (concat  "origin/master.." (magit-get-current-branch)))))

  (magit-define-popup-action 'magit-log-popup
    ?m "origin/master..current" 'magit-log--origin-master)

  (defun magit-diff-range-reversed (rev-or-range &optional args files)
    "Diff between two branches. Unlike `diff-range` works in opposite order i.e.: `base..current`"
    (interactive (list (magit-read-other-branch-or-commit "Diff range")))
    (magit-diff (concat rev-or-range ".." (magit-get-current-branch)) args files))

  (magit-define-popup-action 'magit-diff-popup
    ?R "Diff range (reversed)" 'magit-diff-range-reversed)

  (defun magit-diff--origin-master ()
    "Compare log between branches à la GitHub style between current branch and origin/master"
    (interactive)
    (magit-diff (concat "origin/master.." (magit-get-current-branch))))

  (magit-define-popup-action 'magit-diff-popup
    ?m "origin/master..current" 'magit-diff--origin-master)

  (add-hook 'magit-hook 'turn-off-evil-mc-mode)

  ;; who cares about tags to be displayed in magit-refs buffer?
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

  (custom-set-variables
   '(magit-commit-arguments (quote ("--gpg-sign=CFE12444AF47BD1D")))
   '(magit-fetch-arguments (quote ("--prune")))
   '(magit-log-arguments (quote ("-n500" "--graph" "--color")))))

(with-eval-after-load 'evil-magit
  ;; don't exit magit on escape-sequence and don't bury its buffer on Esc
  (evil-magit-define-key 'normal 'magit-mode-map "<escape>" nil))

(defun ag-version-control/post-init-gist ()
  (setq
   gist-view-gist t ; view your Gist using `browse-url` after it is created
   ))
;;; packages.el ends here
