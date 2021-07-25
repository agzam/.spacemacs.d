;; -*- mode: emacs-lisp; lexical-binding: t -*-
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
                                        (github-review :location (recipe
                                                                  :fetcher github
                                                                  :repo "charignon/github-review"
                                                                  :files ("github-review.el")))
                                        git-link
                                        (gh-notify :location
                                                   (recipe
                                                    :fetcher github
                                                    :repo "anticomputer/gh-notify"
                                                    :branch "dev"))
                                        (forge-visit-here
                                         :location "~/sandbox/forge-visit-here")))

(setq
 vc-follow-symlinks t
 diff-hl-side 'left)

(defun ag-version-control/post-init-magit ()
  (setq
   ;; version-control-diff-tool 'diff-hl
   magit-save-repository-buffers 'dontask
   magit-clone-set-remote.pushDefault nil
   version-control-global-margin t
   magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1
   magit-repository-directories '(("~/work" . 2)
                                  ("~/Sandbox" . 2)
                                  ("~/.hammerspoon" . 1)
                                  ("~/.spacemacs.d" . 1)
                                  ("~/.emacs-profiles/.emacs-spacemacs.d" . 1))
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
    :after forge
    :config
    (dolist (m (list magit-status-mode-map
                     forge-topic-mode-map
                     gh-notify-mode-map))
      (evil-define-key 'normal m (kbd "M-r") 'github-review-forge-pr-at-point)
      (define-key m (kbd "M-r") #'github-review-forge-pr-at-point))

    (defun github-review--copy-suggestion ()
      "kill a region of diff+ as a review suggestion template."
      (interactive)
      (setq deactivate-mark t)
      (let ((s-region
             (buffer-substring-no-properties
              (region-beginning)
              (region-end))))
        (kill-new
         (format "# ```suggestion\n%s\n# ```\n"
                 (replace-regexp-in-string "^\\+" "# " s-region)))))

    (evil-define-key '(visual) github-review-mode-map "M-y" #'github-review--copy-suggestion)

    (defun github-review--after-save-diff (pr-alist diff)
      (let-alist pr-alist
        (with-current-buffer
         (format "%s___%s___%s___%s.diff" .owner .repo .num .sha)
         (goto-line 1))))

    (advice-add 'github-review-save-diff :after 'github-review--after-save-diff)

    ;; otherwise it messes up with backwards-kill-word when commenting
    (bind-key (kbd "M-DEL") nil diff-mode-map)

    (setq github-review-fetch-top-level-and-review-comments t)))

(defun ag-version-control/post-init-git-link ()
  (defun git-link-master-branch ()
    (interactive)
    (require 'git-link)
    (let ((git-link-default-branch "master"))
      (call-interactively #'git-link)))

  (spacemacs/set-leader-keys
    "glm" #'git-link-master-branch))

(defun ag-version-control/init-gh-notify ()
  (use-package gh-notify
    :defer t
    :after magit
    :init (require 'gh-notify)
    :config
    (setq gh-notify-redraw-on-visit t)
    (evil-define-key 'normal gh-notify-mode-map (kbd "RET") 'gh-notify-visit-notification)
    'gh-notify-forge-visit-repo-at-point
    (evil-define-key 'normal gh-notify-mode-map (kbd "q")
      (lambda ()
        (interactive)
        (spacemacs/kill-this-buffer 4)))
    (define-key gh-notify-mode-map (kbd "C-l") nil)
    (spacemacs/set-leader-keys "agh" #'gh-notify)
    (spacemacs/set-leader-keys-for-major-mode 'gh-notify-mode
      "l" #'gh-notify-retrieve-notifications
      "r" #'gh-notify-reset-filter
      "t" #'gh-notify-toggle-timing
      "y" #'gh-notify-copy-url
      "s" #'gh-notify-display-state
      "i" #'gh-notify-ls-issues-at-point
      "p" #'gh-notify-ls-pullreqs-at-point
      "G" #'gh-notify-forge-refresh
      "g" #'gh-notify-forge-visit-repo-at-point
      "m" #'gh-notify-mark-notification
      "M" #'gh-notify-mark-all-notifications
      "u" #'gh-notify-unmark-notification
      "U" #'gh-notify-unmark-all-notifications
      "\\" #'gh-notify-toggle-url-view
      "/d" #'gh-notify-toggle-global-ts-sort
      "/u" #'gh-notify-limit-unread
      "/'" #'gh-notify-limit-repo
      "/\"" #'gh-notify-limit-repo-none
      "/p" #'gh-notify-limit-pr
      "/i" #'gh-notify-limit-issue
      "/*" #'gh-notify-limit-marked
      "/a" #'gh-notify-limit-assign
      "/y" #'gh-notify-limit-author
      "/m" #'gh-notify-limit-mention
      "/t" #'gh-notify-limit-team-mention
      "/s" #'gh-notify-limit-subscribed
      "/c" #'gh-notify-limit-comment
      "/r" #'gh-notify-limit-review-requested
      "//" #'gh-notify-limit-none)

    ;; always recenter when getting back to gh-notify buffer from forge-buffers
    (advice-add 'gh-notify--filter-notifications :after 'recenter)))

(defun ag-version-control/init-forge-visit-here ()
  (use-package forge-visit-here
   :config
    (spacemacs/set-leader-keys "gfL" #'forge-visit-here)))

(with-eval-after-load 'forge
  :config
  (evil-define-key '(normal visual) forge-topic-mode-map "0" #'evil-digit-argument-or-evil-beginning-of-line)
  (evil-define-key '(normal visual) forge-topic-mode-map "$" #'evil-end-of-line)
  (evil-define-key 'normal forge-topic-mode-map "v" #'evil-visual-char)
  (evil-define-key '(normal visual) forge-topic-mode-map "l" #'evil-forward-char)
  (evil-define-key '(normal visual) forge-topic-mode-map "h" #'evil-backward-char)
  (evil-define-key '(normal visual) forge-topic-mode-map "w" #'evil-forward-word-begin)
  (evil-define-key '(normal visual) forge-topic-mode-map "b" #'evil-backward-word-begin))


(with-eval-after-load 'git-gutter-fringe+
  (setq git-gutter-fr+-side 'left-fringe))

;;; packages.el ends here
