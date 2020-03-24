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

(defconst ag-version-control-packages '(magit
                                        gist
                                        (github-review
                                         :recipe (:fetcher github
                                                           :repo "charignon/github-review"
                                                           :files ("github-review.el")))
                                        git-link))

(setq
 vc-follow-symlinks t
 diff-hl-side 'left)

(defun ag-version-control/post-init-magit ()
  (setq
   ;; version-control-diff-tool 'diff-hl
   magit-save-repository-buffers 'dontask
   version-control-global-margin t
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   magit-repository-directories '("~/DevProjects" "~/Sandbox")
   ;; magit-show-refs-arguments '("--sort=-committerdate")
   magit-delete-by-moving-to-trash nil
   magit-branch-rename-push-target nil ; do not push renamed/deleted branch to remote automatically
   magit-diff-refine-hunk 'all
   ;; transient-values-file "~/.spacemacs.d/magit_transient_values.el"

   ;; https://github.com/magit/ghub/issues/81
   gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

  (define-key transient-map "q" #'transient-quit-all)

  ;; Add `git log ORIG_HEAD..HEAD` to magit
  ;; that lets you log only changes since the last pull
  (defun magit-log-orig_head--head (args files)
    "Compare log since the last pull. i.e.: show only commits between last pull and head"
    (interactive (magit-log-arguments))
    (magit-log-other (list "ORIG_HEAD..HEAD") args files))

  (transient-append-suffix 'magit-log "l"
    '("p" "orig_head..head" magit-log-orig_head--head))

  (defun magit-log-other--current (revision)
    "Compare log between branches à la GitHub style.
i.e.: show only commits that differ between selected (other branch) and current branch"
    (interactive (list (magit-read-other-branch-or-commit "Log compare")))
    (magit-log-other (list (concat revision ".." (magit-get-current-branch))) nil nil))

  (transient-append-suffix 'magit-log "l"
    '("R" "other..current" magit-log-other--current))

  (defun magit-log--origin-master ()
    "Compare log between branches à la GitHub style between current branch and origin/master"
    (interactive)
    (magit-log-other (list (concat  "origin/master.." (magit-get-current-branch))) nil nil))

  (transient-append-suffix 'magit-log "l"
    '("m" "origin/master..current" magit-log--origin-master))

  (defun magit-diff-range-reversed (rev-or-range &optional args files)
    "Diff between two branches. Unlike `diff-range` works in opposite order i.e.: `base..current`"
    (interactive (list (magit-read-other-branch-or-commit "Diff range")))
    (magit-diff-range (concat rev-or-range ".." (magit-get-current-branch)) args files))

  (transient-append-suffix 'magit-diff "d"
    '("R" "Diff range (reversed)" magit-diff-range-reversed))

  (defun magit-diff--origin-master ()
    "Compare log between branches à la GitHub style between current branch and origin/master"
    (interactive)
    (magit-diff-range (concat "origin/master.." (magit-get-current-branch))))

  (transient-append-suffix 'magit-diff "d"
    '("m" "origin/master..current" magit-diff--origin-master))

  (add-hook 'magit-hook 'turn-off-evil-mc-mode)

  ;; who cares about tags to be displayed in magit-refs buffer?
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

  )

(with-eval-after-load 'evil-magit
  ;; don't exit magit on escape-sequence and don't bury its buffer on Esc
  (evil-magit-define-key 'normal 'magit-mode-map "<escape>" nil))

(defun ag-version-control/post-init-gist ()
  (setq
   gist-view-gist t ; view your Gist using `browse-url` after it is created
   ))

(defun ag-version-control/init-github-review ()
  (use-package github-review
    :ensure t
    :commands magit-status-mode
    :bind (:map magit-status-mode-map
                ("M-r" . github-review-forge-pr-at-point)
                :map forge-topic-mode-map
                ("M-r" . github-review-forge-pr-at-point))
    :config
    (setq github-review-fetch-top-level-and-review-comments t)))

(defun ag-version-control/post-init-git-link ()
  (defun git-link-master-branch ()
    (interactive)
    (let ((git-link-default-branch "master"))
      (call-interactively 'git-link)))

  (spacemacs/set-leader-keys
    "glm" #'git-link-master-branch))

;;; packages.el ends here
