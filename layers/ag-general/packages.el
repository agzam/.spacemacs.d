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

(defun ag-general/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)))

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

(defun flycheck-mode-off () (flycheck-mode -1))
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode-off)

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
    (interactive (magit-log-arguments))
    (magit-log (list "ORIG_HEAD..HEAD") args files))

  (magit-define-popup-action 'magit-log-popup
    ?p "orig_head..head" 'magit-log-orig_head--head))

(with-eval-after-load 'spacemacs-light-theme
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
   `(diff-refine-removed ((t (:foreground "#d80d0d" :background "#fdb8c0"))))))

(with-eval-after-load 'base16-ocean-dark-theme
  (custom-theme-set-faces
   'base16-ocean-dark
   `(diff-refine-added ((t (:foreground "#a3be70" :background "#2b3b34"))))
   `(diff-refine-removed ((t (:foreground "#ef6160" :background "#3b2c2b"))))))

;;; packages.el ends here
