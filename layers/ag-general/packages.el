;;; packages.el --- ag-general layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-general-packages '(
                                ;; adding magithub explicitly here, until this issue is fixed:
                                ;; see: https://github.com/syl20bnr/spacemacs/issues/9288 and:
                                ;; https://github.com/magit/magit/issues/3154#issuecomment-325648623
                                ;; and wait when they enable this:
                                ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+source-control/github/packages.el#L19
                                magithub
                                helpful
                                rainbow-mode
                                atomic-chrome
                                helm-pages
                                evil-mc))

(defun ag-general/init-magithub ()
  (use-package magithub
    :after magit
    :init
    (setq magithub-dir (concat spacemacs-cache-directory "magithub/"))
    :config
    (progn
      (magithub-feature-autoinject t)
      (define-key magit-status-mode-map "H" #'magithub-dispatch-popup))))

(defun ag-general/init-helpful ()
  (use-package helpful
    :defer t))

(defun ag-general/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)
    ;; remove rectangles from around the colors
    (setq css-fontify-colors nil)))

(defun ag-general/init-atomic-chrome ()
  (use-package atomic-chrome
    :init
    (atomic-chrome-start-server)
    (define-key atomic-chrome-edit-mode-map (kbd "C-c C-c") 'atomic-chrome-close-current-buffer)
    :config
    (setq atomic-chrome-default-major-mode 'markdown-mode
          atomic-chrome-enable-bidirectional-edit t
          atomic-chrome-extension-type-list '(atomic-chrome))
    (add-hook 'atomic-chrome-edit-mode-hook #'ag/atomic-edit-start)
    (add-hook 'atomic-chrome-edit-done-hook #'ag/atomic-edit-done)))

(defun ag-general/init-helm-pages ()
  (use-package helm-pages
    :defer t))

(with-eval-after-load 'flycheck
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (defun flycheck-mode-off () (flycheck-mode -1))
    (add-hook 'emacs-lisp-mode-hook #'flycheck-mode-off)))

(with-eval-after-load 'ibuf-ext
  (setq
    ibuffer-old-time 8 ;; buffer considered old after that many hours
    ibuffer-group-buffers-by 'projects
    ibuffer-expert t
    ibuffer-show-empty-filter-groups nil)

  (define-ibuffer-filter unsaved-file-buffers
    "Toggle current view to buffers whose file is unsaved."
  (:description "file is unsaved")
  (ignore qualifier)
  (and (buffer-local-value 'buffer-file-name buf)
       (buffer-modified-p buf)))

  (define-ibuffer-filter file-buffers
      "Only show buffers backed by a file."
    (:description "file buffers")
    (ignore qualifier)
    (buffer-local-value 'buffer-file-name buf))

  (define-key ibuffer-mode-map (kbd "/ u") #'ibuffer-filter-by-unsaved-file-buffers)
  (define-key ibuffer-mode-map (kbd "/ F") #'ibuffer-filter-by-file-buffers))

;; Taken from the discussion here: https://github.com/syl20bnr/spacemacs/issues/2669
;; TODO: remove this when official method is implemented in Spacmacs
(defun ag-general/init-evil-mc ()
  (use-package evil-mc
    :config
    (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
    (add-hook 'text-mode-hook 'turn-on-evil-mc-mode)

    (defun evil--mc-make-cursor-at-col (startcol _endcol orig-line)
      (move-to-column startcol)
      (unless (= (line-number-at-pos) orig-line)
        (evil-mc-make-cursor-here)))
    (defun evil-mc-make-vertical-cursors (beg end)
      (interactive (list (region-beginning) (region-end)))
      (evil-mc-pause-cursors)
      (apply-on-rectangle #'evil--mc-make-cursor-at-col
                          beg end (line-number-at-pos (point)))
      (evil-mc-resume-cursors)
      (evil-normal-state)
      (move-to-column (evil-mc-column-number (if (> end beg)
                                                 beg
                                               end))))))

;; Add `git log ORIG_HEAD..HEAD` to magit
;; that lets you log only changes since the last pull
(with-eval-after-load 'magit
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

  (defun magit-diff-range-reversed (rev-or-range &optional args files)
    "Diff between two branches. Unlike `diff-range` works in opposite order i.e.: `base..current`"
    (interactive (list (magit-read-other-branch-or-commit "Diff range")))
    (magit-diff (concat rev-or-range ".." (magit-get-current-branch)) args files))

  (magit-define-popup-action 'magit-diff-popup
    ?R "Diff range (reversed)" 'magit-diff-range-reversed))

(defun ag/decrease-powerline-fonts (&optional theme)
  "Slightly decrease elements of the powerline, which-key and minibuffer"
  (custom-theme-set-faces
   (or theme spacemacs--cur-theme)
   `(powerline ((t (:height 0.9))))
   `(powerline-active0 ((t (:height 0.9))))
   `(powerline-active1 ((t (:height 0.9))))
   `(powerline-active2 ((t (:height 0.9))))
   `(powerline-inactive0 ((t (:height 0.9))))
   `(powerline-inactive1 ((t (:height 0.9))))
   `(powerline-inactive2 ((t (:height 0.9))))
   `(mode-line ((t (:height 0.9))))
   `(mode-line-inactive ((t (:height 0.9))))
   `(mode-line-highlight ((t (:height 0.9))))
   `(mode-line-buffer-id ((t (:height 0.9))))
   `(mode-line-buffer-id-inactive ((t (:height 0.9))))
   `(mode-line-emphasis ((t (:height 0.9))))

   `(spacemacs-micro-state-header-face ((t (:height 0.9))))
   `(spacemacs-micro-state-binding-face ((t (:height 0.9))))
   `(spacemacs-transient-state-title-face ((t (:height 0.9))))

   `(persp-face-lighter-buffer-not-in-persp ((t (:height 0.9))))
   `(persp-face-lighter-default ((t (:height 0.9))))
   `(persp-face-lighter-nil-persp ((t (:height 0.9)))))

  (dolist (buf (list " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*" " *which-key*"))
    (when (get-buffer buf)
      (with-current-buffer buf
        (setq-local face-remapping-alist '((default (:height 0.8))))))))

(defun ag/adjust-themes ()
  (pcase spacemacs--cur-theme
    ('spacemacs-light
     (progn
       (custom-theme-set-faces
        'spacemacs-light
        `(magit-diff-hunk-heading ((t (:background "#efeae9"))))
        `(magit-diff-hunk-heading-highlight ((t (:background "#efeae9"))))
        `(magit-diff-context-highlight ((t (:background "#fbf8ef"))))
        `(magit-diff-added ((t (:foreground "#67963d" :background "#e6ffed"))))
        `(magit-diff-added-highlight ((t (:foreground "#325e0b" :background "#e6ffed"))))
        `(magit-diff-removed ((t (:foreground "#ef6160" :background "#ffeef0"))))
        `(magit-diff-removed-highlight ((t (:foreground "#d80d0d" :background "#ffeef0"))))
        `(diff-refine-added ((t (:foreground "#325e0b" :background "#acf2bd"))))
        `(diff-refine-removed ((t (:foreground "#d80d0d" :background "#fdb8c0"))))
        `(ahs-plugin-whole-buffer-face ((t (:background "#e5e1e0")))))
       (ag/decrease-powerline-fonts 'spacemacs-light)))

    ('base16-ocean-dark
     (progn
       (custom-theme-set-faces
        'base16-ocean-dark
        `(magit-section-highlight ((t (:background "#2f343f"))))
        `(magit-diff-hunk-heading ((t (:background "#2f343f"))))
        `(magit-diff-hunk-heading-highlight ((t (:background "#2f363f"))))
        `(diff-refine-added ((t (:foreground "#a3be70" :background "#2b3b34"))))
        `(diff-refine-removed ((t (:foreground "#ef6160" :background "#3b2c2b")))))
       (ag/decrease-powerline-fonts 'base16-ocean-dark)))))

(with-eval-after-load 'core-themes-support
  (ag/adjust-themes)
  (add-hook 'spacemacs-post-theme-change-hook 'ag/adjust-themes t))

;;; packages.el ends here
