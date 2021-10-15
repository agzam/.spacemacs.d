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

(defun ag/decrease-powerline-fonts (&optional theme)
  "Slightly decrease elements of the powerline, which-key and minibuffer"
  (let ((faces '(mode-line
                 mode-line-buffer-id
                 mode-line-emphasis
                 mode-line-highlight
                 mode-line-inactive

                 persp-face-lighter-default
                 persp-face-lighter-nil-persp
                 persp-face-lighter-buffer-not-in-persp

                 eyebrowse-mode-line-active
                 eyebrowse-mode-line-inactive
                 eyebrowse-mode-line-separator
                 eyebrowse-mode-line-delimiters

                 doom-modeline-buffer-timemachine
                 doom-modeline-battery-error
                 doom-modeline-battery-critical
                 doom-modeline-battery-warning
                 doom-modeline-battery-normal
                 doom-modeline-battery-full
                 doom-modeline-battery-charging
                 doom-modeline-lsp-running
                 doom-modeline-lsp-error
                 doom-modeline-lsp-warning
                 doom-modeline-lsp-success
                 doom-modeline-repl-warning
                 doom-modeline-repl-success
                 doom-modeline-persp-buffer-not-in-persp
                 doom-modeline-persp-name
                 doom-modeline-evil-replace-state
                 doom-modeline-evil-visual-state
                 doom-modeline-evil-operator-state
                 doom-modeline-evil-normal-state
                 doom-modeline-evil-motion-state
                 doom-modeline-evil-insert-state
                 doom-modeline-evil-emacs-state
                 doom-modeline-debug-visual
                 doom-modeline-bar-inactive
                 doom-modeline-bar
                 doom-modeline-unread-number
                 doom-modeline-notification
                 doom-modeline-urgent
                 doom-modeline-warning
                 doom-modeline-info
                 doom-modeline-debug
                 doom-modeline-input-method-alt
                 doom-modeline-input-method
                 doom-modeline-host
                 doom-modeline-panel
                 doom-modeline-highlight
                 doom-modeline-project-root-dir
                 doom-modeline-project-dir
                 doom-modeline-project-parent-dir
                 doom-modeline-buffer-minor-mode
                 doom-modeline-buffer-major-mode
                 doom-modeline-buffer-modified
                 doom-modeline-buffer-file
                 doom-modeline-buffer-path
                 doom-modeline-vspc-face
                 doom-modeline-spc-face)))
    (dolist (f faces)
      (when (facep f)
        (set-face-attribute f nil :height 1)
        (set-face-attribute f nil :height 0.85))))

  (dolist (buf (list " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*" " *which-key*"))
    (when (get-buffer buf)
      (with-current-buffer buf
        (setq-local face-remapping-alist '((default (:height 0.85))))))))

(defun ag/set-faces-attributes (faces)
  "Sets face attributes for given alist of FACES"
  (dolist (f faces)
    (let* ((face (car f))
           (attribs (cdr f))
           (params (cons face (cons nil attribs))))
      (when (facep face)
        (set-face-attribute face nil :background nil :foreground nil)
        (apply 'set-face-attribute params)))))

(defun ag/adjust-base16-ocean-colors ()
  (let* ((base00 "#242938")
         (base00*1 "#212533")
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
         (base10 "#7090af")
         (default-background base00)
         (default-foreground base05)
         (faces `(;; magit
                  (magit-popup-disabled-argument . (:foreground ,base02))
                  (magit-popup-option-value . (:foreground ,base08))
                  (magit-popup-argument . (:foreground ,base08))

                  (magit-diff-context-highlight . (:background ,default-background))
                  (magit-diff-removed . (:foreground ,base08))
                  (magit-diff-added . (:foreground ,base0B))
                  (magit-diff-removed-highlight . (:foreground "#ef6160"))
                  (magit-diff-added-highlight . (:foreground "#a3be70"))
                  (magit-section-highlight . (:background "#2f343f"))
                  (magit-diff-hunk-heading . (:background "#2f343f"))
                  (magit-diff-hunk-heading-highlight . (:background "#2f363f"))
                  (diff-refine-added . (:foreground "#a3be70" :background "#2b3b34"))
                  (diff-refine-removed . (:foreground "#ef6160" :background "#3b2c2b"))
                  (smerge-refined-added . (:foreground "#a3be70" :background "#2b3b34"))
                  (smerge-refined-removed . (:foreground "#ef6160" :background "#3b2c2b"))

                  (ediff-current-diff-A . (:foreground "#dd828b" :background "#443238"))
                  (ediff-fine-diff-A . (:foreground "#db5e6c" :background "#603238"))
                  (ediff-current-diff-B . (:foreground ,base0B :background "#2a3a2c"))
                  (ediff-fine-diff-B . (:foreground "#aadd7e" :background "#2e4431"))

                  ;; diff-hl
                  (diff-hl-change . (:foreground ,base03 :background ,base0D))
                  (diff-hl-delete . (:foreground ,base03 :background ,base08))
                  (diff-hl-insert . (:foreground ,base03 :background ,base0B))
                  (diff-hl-unknown . (:foreground ,base03 :background ,base0A))

                  (ahs-plugin-whole-buffer-face . (:foreground ,base0B :background ,default-background))
                  (ahs-plugin-default-face . (:foreground ,base0A :background ,base02))
                  (ahs-plugin-default-face-unfocused . (:inherit ahs-plugin-default-face))
                  (ahs-definition-face . (:foreground ,base0A :background ,base03))
                  (ahs-face . (:foreground ,base0A :background ,base02))

                  (region . (:inverse-video t :foreground ,base03 :background ,default-background :distant-foreground nil))
                  (fixed-pitch . (:family "JetBrains Mono" :weight normal :width expanded))
                  (variable-pitch . (:family "Open Sans" :weight normal
                                             :width expanded :height 1.2))
                  ;; avy
                  (aw-leading-char-face . (:height 5.0 :foreground "Orange"))
                  (avy-lead-face . (:height 1.3 :foreground ,base0A))
                  (avy-lead-face-0 . (:height 1.3 :foreground ,base09))
                  (avy-lead-face-1 . (:height 1.3 :foreground ,base0C))
                  (avy-lead-face-2 . (:height 1.3 :foreground ,base10))

                  ;; helm
                  (helm-swoop-target-line-face . (:foreground ,base04 :background ,base02))
                  (helm-swoop-target-word-face . (:foreground ,base0A :background ,base02 :weight bold))
                  (helm-swoop-target-line-block-face . (:background ,base0B :foreground ,base01))

                  ;; org-mode
                  (org-link . (:underline t :foreground ,base0B))
                  (org-todo . (:weight bold :foreground ,base0A :inherit fixed-pitch))
                  (org-block-begin-line . (:underline nil :background ,base01 :foreground ,base04 :height 0.9 :weight ultra-light :inherit fixed-pitch))
                  (org-block . (:background ,base01 :inherit fixed-pitch))
                  (org-block-end-line . (:overline nil :background ,base01 :height 0.9 :weight ultra-light :inherit fixed-pitch))
                  (org-verse . (:inherit (variable-pitch org-block)))
                  (org-quote . (:inherit (org-verse) :slant normal))
                  (org-date . (:inherit fixed-pitch))
                  (org-code . (:inherit fixed-pitch :foreground ,base06))
                  (org-verbatim . (:inherit fixed-pitch))
                  (org-meta-line . (:inherit fixed-pitch :foreground ,base04 :height 0.9 :weight ultra-light))
                  (org-checkbox . (:background nil :inherit fixed-pitch))
                  (org-table . (:inherit fixed-pitch))
                  (org-level-1 . (:foreground ,base0D :bold t :height 1.3))
                  (org-level-2 . (:foreground ,base09 :bold t :height 1.2))
                  (org-level-3 . (:foreground ,base0B :height 1.1))
                  (org-level-4 . (:foreground ,base10 :height 1.0))
                  (org-level-5 . (:foreground ,base0E :height 1.0))
                  (org-level-6 . (:foreground ,base0C :height 1.0))
                  (org-level-7 . (:foreground ,base07 :height 1.0))
                  (org-level-8 . (:foreground ,base0D :height 1.0))
                  (org-done . (:foreground ,base02))
                  (org-headline-done . (:foreground ,base02))
                  (org-hide . (:foreground ,default-background))
                  (org-indent . (:inherit (fixed-pitch org-hide)))
                  (org-roam-link . (:background ,base01 :foreground ,base0D))
                  (org-roam-link-invalid . (:background "#45352e"))
                  (org-drawer . (:foreground ,base02))
                  (org-special-keyword . (:inherit (org-drawer fixed-pitch) :foreground ,base02))
                  (org-property-value . (:inherit (org-drawer fixed-pitch) :foreground ,base02))

                  ;; code
                  (font-lock-doc-face . (:foreground ,base02))

                  ;; misc
                  (hl-line . (:background "#2f3440"))
                  (trailing-whitespace . (:background ,base01))
                  (mode-line . (:underline (:color ,base01)))
                  (mode-line-inactive . (:underline (:color ,base01)))
                  (default . (:background ,default-background :foreground ,default-foreground))

                  ;; (mode-line . (:height 0.7))
                  ;; (mode-line-inactive . (:height 1))
                  ;; (mode-line-emphasis . (:weight normal))

                  (line-number . (:inherit fixed-pitch))
                  (ivy-posframe . (:inherit default))

                  (notmuch-wash-cited-text . (:foreground ,base03))
                  (message-header-to . (:foreground ,base0C))
                  (notmuch-crypto-signature-unknown . (:foreground ,base0E :background nil))
                  (notmuch-crypto-signature-good-key . (:foreground ,base0A :background nil)))))
    (ag/set-faces-attributes faces)
    (setq pdf-view-midnight-colors `(,base04 . ,default-background))))

(defun ag/adjust-spacemacs-light-colors ()
  (let* ((default-background "#fbf8ef")
         (default-foreground "#655370")
         (bg-darker "#f7f4eb")
         (bg-accent "#dedae0")
         (bg-accent-dark "#c7c1c9")
         (bg-accent-darker "#b1adb3")
         (bg-accent-light "#efedf0")
         (fg-accent "CadetBlue")
         (faces `((region . (:inverse-video t :foreground ,bg-accent :background ,default-background :distant-foreground nil))
                  (fringe . (:background ,bg-darker))
                  (magit-diff-hunk-heading . (:background ,bg-darker))
                  (magit-diff-hunk-heading-highlight . (:background ,bg-accent-light))
                  (magit-diff-context-highlight . (:background ,bg-darker))
                  (magit-diff-added . (:foreground "#67963d" :background "#e6ffed"))
                  (magit-diff-added-highlight . (:foreground "#325e0b" :background "#e6ffed"))
                  (magit-diff-removed . (:foreground "#ef6160" :background "#ffeef0"))
                  (magit-diff-removed-highlight . (:foreground "#d80d0d" :background "#ffeef0"))
                  (diff-refine-added . (:foreground "#325e0b" :background "#acf2bd"))
                  (diff-refine-removed . (:foreground "#d80d0d" :background "#fdb8c0"))
                  (smerge-upper . (:foreground "#d80d0d" :background "#fdb8c0"))
                  (smerge-lower . (:foreground "#325e0b" :background "#acf2bd"))

                  (trailing-whitespace . (:background ,bg-accent))
                  (ahs-face . (:background ,bg-accent-light))
                  (ahs-plugin-default-face . (:background ,bg-accent-light))
                  (ahs-plugin-default-face-unfocused . (:inherit ahs-plugin-default-face))
                  (ahs-definition-face . (:background "#e6ffed"))
                  (ahs-plugin-whole-buffer-face . (:foreground ,bg-accent :inverse-video t :background ,default-background))
                  (evil-ex-lazy-highlight . (:background ,bg-accent))
                  (evil-ex-search . (:background "DarkKhaki"))
                  (aw-leading-char-face . (:height 5.0))
                  (mode-line . (:underline (:color "#b2b2b2")))
                  (mode-line-inactive . (:underline (:color "#d3d3e7")))

                  (fixed-pitch . (:family "JetBrains Mono" :weight normal :width expanded))
                  (variable-pitch . (:family "Open Sans" :weight normal
                                             :width expanded :height 1.2))

                  ;; (lsp-lens-face . (:foreground ,default-foreground))
                  (lsp-ui-peek-list . (:background "#f0ece1" :foreground "#866f94"))
                  (lsp-ui-peek-peek . (:background "#f0ece1"))
                  (lsp-ui-peek-footer . (:background ,bg-accent))
                  (lsp-ui-peek-header . (:background ,bg-accent))
                  (lsp-ui-peek-highlight . (:background "#f5f1e6" :foreground "#8b7b96" :box nil))
                  (lsp-ui-peek-filename . (:background "#f0ece1" :foreground "#c0a9cf"))
                  (lsp-ui-peek-selection . (:background ,bg-accent))

                  (lsp-ui-sideline-current-symbol . (:foreground ,default-foreground))
                  (lsp-ui-sideline-symbol . (:foreground ,default-foreground))
                  (lsp-ui-sideline-code-action . (:foreground "#b7b1ba"))
                  (lsp-ui-doc-background . (:background "#f0ece1"))
                  (markdown-code-face . (:foreground ,default-foreground))
                  (cider-debug-code-overlay-face . (:background "#f0ece1"))
                  ;; (lsp-ui-sideline-global . (:foreground ,default-foreground))

                  ;; org-mode
                  (org-block-begin-line . (:background ,bg-darker :foreground ,fg-accent :height 0.9 :weight ultra-light :inherit fixed-pitch))
                  (org-block . (:background ,bg-darker :inherit fixed-pitch))
                  (org-block-end-line . (:background ,bg-darker :foreground ,fg-accent :height 0.9 :weight ultra-light :inherit fixed-pitch))
                  (org-verse . (:inherit (variable-pitch org-block)))
                  (org-quote . (:inherit (org-verse) :slant normal))
                  (org-date . (:inherit fixed-pitch))
                  (org-code . (:inherit fixed-pitch :foreground ,fg-accent))
                  (org-verbatim . (:inherit fixed-pitch))
                  (info-quoted-name . (:inherit fixed-pitch))
                  (org-meta-line . (:inherit fixed-pitch :foreground ,fg-accent :height 0.9 :weight ultra-light))
                  (org-special-keyword . (:inherit fixed-pitch))
                  (org-checkbox . (:inherit fixed-pitch))
                  (org-table . (:inherit fixed-pitch))
                  (org-done .  (:foreground ,bg-accent))
                  (org-headline-done . (:foreground ,bg-accent-dark))
                  (org-hide . (:foreground ,default-background))
                  (org-indent . (:inherit (fixed-pitch org-hide)))
                  (org-roam-link . (:background "#fff9de"))
                  (org-roam-link-invalid . (:background "#fbf3ef"))
                  (org-drawer . (:foreground ,bg-accent-dark))
                  (org-special-keyword . (:inherit (org-drawer) :foreground ,bg-accent-dark))
                  (org-property-value . (:inherit (org-drawer) :foreground ,bg-accent-dark))

                  ;; (org-level-1 . (:inherit variable-pitch :bold t :height 1.3))
                  ;; (org-level-2 . (:inherit variable-pitch :bold t :height 1.2))
                  ;; (org-level-3 . (:inherit variable-pitch :height 1.1))
                  ;; (org-level-4 . (:inherit variable-pitch :height 1.0))
                  ;; (org-level-5 . (:inherit variable-pitch :height 1.0))
                  ;; (org-level-6 . (:inherit variable-pitch :height 1.0))
                  ;; (org-level-7 . (:inherit variable-pitch :height 1.0))
                  ;; (org-level-8 . (:inherit variable-pitch :height 1.0))

                  (gnus-cite-1 . (:foreground "SkyBlue3"))
                  (gnus-cite-2 . (:foreground "light sky blue"))
                  (gnus-cite-3 . (:foreground "yellow3"))
                  (mm-uu-extract . (:background "#efeae9"))

                  ;; (mode-line . (:height 0.7))
                  ;; (mode-line-inactive . (:height 1))
                  ;; (mode-line-emphasis . (:weight normal))

                  (cider-debug-code-overlay-face . (:background ,bg-darker))
                  (ivy-posframe . (:inherit default))

                  (notmuch-wash-cited-text . (:foreground ,bg-accent-darker))
                  (message-header-to . (:foreground ,fg-accent))
                  (notmuch-crypto-signature-unknown . (:foreground "#fdb8c0" :background nil))
                  (notmuch-crypto-signature-good-key . (:foreground "DarkKhaki" :background nil)))))
    (ag/set-faces-attributes faces)))

(defun ag/adjust-themes ()
  (ag/decrease-powerline-fonts)
  (pcase spacemacs--cur-theme
    ('spacemacs-light (ag/adjust-spacemacs-light-colors))
    ('base16-ocean (ag/adjust-base16-ocean-colors))))

(defun ag-colors/init-base16-theme ()
  (use-package base16-theme))

;; this is a workaround the bug where Emacs doesn't correctly use the current
;; theme with new frames
;; https://github.com/syl20bnr/spacemacs/issues/11916
(defun ag/new-frame-init (frame)
  (run-at-time "0.002 sec" nil
               (lambda ()
                 (enable-theme spacemacs--cur-theme)
                 (ag/adjust-themes))))

(with-eval-after-load 'core-themes-support
  (add-hook 'spacemacs-post-theme-change-hook 'ag/adjust-themes t)
  (add-hook 'after-make-frame-functions 'ag/new-frame-init))

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
