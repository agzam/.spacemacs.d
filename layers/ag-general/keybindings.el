;;; keybindings.el --- ag-general layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;;; disable nonsensical keys
(dolist (key '("s-n" "s-p" "s-q" "s-m" "H-q" "C-x C-c"))
  (unbind-key (kbd key)))

;;;; don't quit helpful on Esc
(evil-define-key 'motion help-mode-map (kbd "<escape>") nil)

(evil-define-key 'normal diff-mode-map "q" #'quit-window)

(if (configuration-layer/layer-used-p 'helm)
    (progn
      (global-set-key (kbd "s-B") #'lazy-helm/helm-mini)
      (global-set-key (kbd "s-b") #'spacemacs-layouts/non-restricted-buffer-list-helm)
      (global-set-key (kbd "H-B") #'lazy-helm/helm-mini)
      (global-set-key (kbd "H-b") #'spacemacs-layouts/non-restricted-buffer-list-helm)
      (define-key helm-map (kbd "C-c M-i") #'helm-copy-to-buffer))
  (progn
    (global-set-key (kbd "s-B") #'ivy-switch-buffer)
    (global-set-key (kbd "s-b") #'spacemacs-layouts/non-restricted-buffer-list-ivy)
    (global-set-key (kbd "H-B") #'ivy-switch-buffer)
    (global-set-key (kbd "H-b") #'spacemacs-layouts/non-restricted-buffer-list-ivy)

    (define-key ivy-minibuffer-map (kbd "C-c RET") #'ivy-rotate-preferred-builders)

    (evil-define-key '(normal evilified) ivy-occur-grep-mode-map
      "n" #'evil-ex-search-next
      "p" #'evil-ex-search-previous)

    (evil-define-key '(normal evilified) ivy-occur-mode-map
      "gg" #'evil-goto-first-line
      "gr" #'ivy-occur-revert-buffer)

    ;; open in other-window action
    (define-key ivy-minibuffer-map
      (kbd "M-l")
      (lambda () (interactive)
        (execute-kbd-macro (kbd "M-o j"))))))

(global-set-key (kbd "s-[") #'spacemacs/persp-go-prev)
(global-set-key (kbd "s-]") #'spacemacs/persp-go-next)
(global-set-key (kbd "H-[") #'spacemacs/persp-go-prev)
(global-set-key (kbd "H-]") #'spacemacs/persp-go-next)

;;;; restore vanilla universal argument keybinding. Spacemacs redefines it
(define-key evil-normal-state-map (kbd "C-u") 'universal-argument)

;;;; wrap `sp-up-sexp` "Move forward out of one level of parentheses", so it can be used in evil-lispy
(with-eval-after-load 'evil-lisp-state
  (evil-lisp-state-enter-command sp-up-sexp)

  (defun sp-reindent ()
    (interactive)
    (save-excursion
      (er/expand-region 2)
      (evil-indent (region-beginning) (region-end))))

  (evil-lisp-state-enter-command sp-reindent)
  (spacemacs/set-leader-keys
    "kf" #'evil-lisp-state-sp-up-sexp
    "k=" #'evil-lisp-state-sp-reindent))

;;;;;;;;;;;
;; Shell ;;
;;;;;;;;;;;

(defun eshell-keybindings-override ()
  (define-key eshell-mode-map (kbd "C-c C-l")
    (lambda ()
      (interactive)
      (eshell/clear-scrollback)
      (eshell-reset)))
  (evil-define-key 'normal eshell-mode-map
    "RET" 'eshell-action-on-file-or-dir-at-point
    "o" 'eshell-action-on-file-or-dir-at-point-other-window)
  (evil-define-key 'insert eshell-mode-map
    (kbd "C-p") 'eshell-previous-matching-input-from-input
    (kbd "C-n") 'eshell-next-maching-input-from-input))

(with-eval-after-load 'evil
  (evil-define-key '(normal insert) shell-mode-map
    (kbd "C-l") #'comint-clear-buffer
    (kbd "C-C C-l") #'shell-history)
  (evil-define-key '(normal insert) eshell-mode-map
    (kbd "C-C C-l") #'shell-history))

(add-hook 'eshell-mode-hook 'eshell-keybindings-override t)

;;;;;;;;;;
;; Misc ;;
;;;;;;;;;;

(spacemacs/set-leader-keys
  "qq" nil  ; no unexpected exits
  "qQ" #'spacemacs/prompt-kill-emacs
  "s/" #'engine/search-google
  "jj" #'avy-goto-char-timer
  "ja" #'beginning-of-defun
  "je" #'end-of-defun
  "swg" #'helm-google-suggest
  ;; "ou" #'spacemacs/avy-open-url
  ;;;; add a page-break
  "iP" (kbd "i C-q C-l <RET><escape>")
  "nn" #'global-display-line-numbers-mode)

(spacemacs/transient-state-register-add-bindings 'zoom-frm
  '(("m" (toggle-frame-maximized-undecorated))))

(evil-define-key '(normal evilified) global-map
  (kbd "s-a") #'mark-whole-buffer
  (kbd "C-S-e") (lambda () (interactive) (scroll-other-window 1))
  (kbd "C-S-y") (lambda () (interactive) (scroll-other-window-down 1)))

;;;; ---------------
;;;; Smartparens
;;;; ---------------
(dolist (m '(lisp-mode-map clojure-mode-map cider-repl-mode-map))
  (evil-define-key 'insert (symbol-value m)
    (kbd "M-l") #'sp-forward-slurp-sexp
    (kbd "M-h") #'sp-forward-barf-sexp
    (kbd "M-L") #'sp-backward-slurp-sexp
    (kbd "M-H") #'sp-backward-barf-sexp))

;;;; making Info-mode keys more suitable for Evil
(define-key Info-mode-map (kbd "H") #'Info-up)
(define-key Info-mode-map (kbd "C-o") #'Info-prev)
(define-key Info-mode-map (kbd "C-i") #'Info-next)
(define-key Info-mode-map (kbd "C-j") #'Info-goto-node)
(define-key Info-mode-map (kbd "C-M-j") #'Info-goto-node-web)
(define-key Info-mode-map (kbd "q") (lambda () (interactive) (Info-exit :kill-window)))
(dolist (key '("n" "p" "g" "G"))
  (unbind-key (kbd key) 'Info-mode-map))


;;;; -----------
;;;; Company
;;;; -----------
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-f") #'company-filter-candidates))

;;;;;;;;;;;;;;;;;;;;
;; xwidget-webkit ;;
;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'xwidget
  (evil-define-key '(normal evilified) xwidget-webkit-mode-map
    "q" 'quit-window
    "k" 'xwidget-webkit-scroll-down-line
    "j" 'xwidget-webkit-scroll-up-line
    "h" 'xwidget-webkit-scroll-backward
    "l" 'xwidget-webkit-scroll-forward
    (kbd "C-f") 'xwidget-webkit-scroll-up
    (kbd "C-b") 'xwidget-webkit-scroll-down
    "+" 'xwidget-webkit-zoom-in
    "=" 'xwidget-webkit-zoom-in
    "-" 'xwidget-webkit-zoom-out
    "R" 'xwidget-webkit-reload
    "gr" 'xwidget-webkit-reload
    "H" 'xwidget-webkit-back
    "L" 'xwidget-webkit-forward
    "gu" 'xwidget-webkit-browse-url
    "gg" 'xwidget-webkit-scroll-top
    "G" 'xwidget-webkit-scroll-bottom
    "y" 'xwidget-webkit-copy-selection-as-kill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors, evil-mc ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'evil-mc
  ;; add cursors on right-click
  (global-set-key (kbd "<mouse-3>") 'evil-mc-toggle-cursor-on-click))

;;; keybindings.el ends here
