;;; packages.el --- ag-colors layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-colors-packages '(base16-theme))

(defun ag-colors/init-base16-theme ()
  (use-package base16-theme))

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

   `(which-key-docstring-face ((t (:height 0.9))))
   `(which-key-group-description-face ((t (:height 0.9))))
   `(which-key-command-description-face ((t (:height 0.9))))
   `(which-key-local-map-description-face ((t (:height 0.9))))

   `(spacemacs-micro-state-header-face ((t (:height 0.9))))
   `(spacemacs-micro-state-binding-face ((t (:height 0.9))))
   `(spacemacs-transient-state-title-face ((t (:height 0.9))))

   `(persp-face-lighter-buffer-not-in-persp ((t (:height 0.9))))
   `(persp-face-lighter-default ((t (:height 0.9))))
   `(persp-face-lighter-nil-persp ((t (:height 0.9)))))

  (dolist (buf (list " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*" " *which-key*"))
    (when (get-buffer buf)
      (with-current-buffer buf
        (setq-local face-remapping-alist '((default (:height 0.9))))))))

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
        `(trailing-whitespace ((t (:background "#e5e1e0"))))
        `(ahs-plugin-whole-buffer-face ((t (:background "#e5e1e0"))))
        `(aw-leading-char-face ((t (:height 5.0)))))
       (ag/decrease-powerline-fonts 'spacemacs-light)))

    ('base16-ocean
     (progn
       (let ((base00 "#2b303b")
             (base01 "#343d46")
             (base02 "#4f5b66")
             (base03 "#65737e")
             (base04 "#a7adba")
             (base05 "#c0c5ce")
             (base06 "#dfe1e8")
             (base07 "#eff1f5")
             (base08 "#bf616a")
             (base09 "#d08770")
             (base0A "#ebcb8b")
             (base0B "#a3be8c")
             (base0C "#96b5b4")
             (base0D "#8fa1b3")
             (base0E "#b48ead")
             (base0F "#ab7967")
             (base10 "#7090af"))
         (custom-theme-set-faces
          'base16-ocean

          ;; magit
          `(magit-popup-disabled-argument ((t (:foreground ,base02))))
          `(magit-popup-option-value ((t (:foreground ,base08))))
          `(magit-popup-argument ((t (:foreground ,base08))))

          `(magit-diff-context-highlight ((t (:background ,base00))))
          `(magit-diff-removed ((t (:foreground ,base08))))
          `(magit-diff-added ((t (:foreground ,base0B))))
          `(magit-diff-removed-highlight ((t (:foreground "#ef6160"))))
          `(magit-diff-added-highlight ((t (:foreground "#a3be70"))))
          `(magit-section-highlight ((t (:background "#2f343f"))))
          `(magit-diff-hunk-heading ((t (:background "#2f343f"))))
          `(magit-diff-hunk-heading-highlight ((t (:background "#2f363f"))))
          `(diff-refine-added ((t (:foreground "#a3be70" :background "#2b3b34"))))
          `(diff-refine-removed ((t (:foreground "#ef6160" :background "#3b2c2b"))))

          `(ediff-current-diff-A ((t (:foreground "#dd828b" :background "#443238"))))
          `(ediff-fine-diff-A ((t (:foreground "#db5e6c" :background "#603238"))))
          `(ediff-current-diff-B ((t (:foreground ,base0B :background "#2a3a2c"))))
          `(ediff-fine-diff-B ((t (:foreground "#aadd7e" :background "#2e4431"))))

          ;; diff-hl
          `(diff-hl-change ((t (:foreground ,base03 :background ,base0D))))
          `(diff-hl-delete ((t (:foreground ,base03 :background ,base08))))
          `(diff-hl-insert ((t (:foreground ,base03 :background ,base0B))))
          `(diff-hl-unknown ((t (:foreground ,base03 :background ,base0A))))

          `(ahs-plugin-whole-buffer-face ((t (:foreground ,base0B :background ,base00))))
          `(ahs-face ((t (:foreground ,base0A :background ,base02))))

          ;; avy
          `(aw-leading-char-face ((t (:height 5.0 :foreground "Orange"))))
          `(avy-lead-face ((t (:height 1.3 :foreground ,base0A))))
          `(avy-lead-face-0 ((t (:height 1.3 :foreground ,base09))))
          `(avy-lead-face-1 ((t (:height 1.3 :foreground ,base0C))))
          `(avy-lead-face-2 ((t (:height 1.3 :foreground ,base10))))

          ;; helm
          `(helm-swoop-target-line-face ((t (:foreground ,base04 :background ,base02))))
          `(helm-swoop-target-word-face ((t (:foreground ,base0A :background ,base02 :weight bold))))
          `(helm-swoop-target-line-block-face ((t (:background ,base0B :foreground ,base01))))

          ;; org-mode
          `(org-todo ((t (:weight bold :foreground ,base0A))))
          `(org-done ((t (:strike-through ,base0D))))
          `(org-block-begin-line ((t (:underline ,base02 :foreground ,base04 :height 0.8 :weight ultra-light))))
          `(org-block-end-line ((t (:overline ,base02 :foreground ,base04 :height 0.8 :weight ultra-light))))
          `(org-level-1 ((t (:foreground ,base0D :bold t :height 1.3))))
          `(org-level-2 ((t (:foreground ,base09 :bold t :height 1.2))))
          `(org-level-3 ((t (:foreground ,base0B :height 1.1))))
          `(org-level-4 ((t (:foreground ,base10 :height 1.0))))
          `(org-level-5 ((t (:foreground ,base0E :height 1.0))))
          `(org-level-6 ((t (:foreground ,base0C :height 1.0))))
          `(org-level-7 ((t (:foreground ,base07 :height 1.0))))
          `(org-level-8 ((t (:foreground ,base0D :height 1.0))))

          ;; code
          `(font-lock-doc-face ((t (:foreground ,base02))))

          ;; misc
          `(hl-line ((t (:background "#2f3440"))))
          `(trailing-whitespace ((t (:background ,base01))))
          `(default ((t (:background ,base00 :foreground ,base05))))) )
       (ag/decrease-powerline-fonts 'base16-ocean)))))

(with-eval-after-load 'core-themes-support
  (ag/adjust-themes)
  (add-hook 'spacemacs-post-theme-change-hook 'ag/adjust-themes t))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
