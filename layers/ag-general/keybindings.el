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
(dolist (key '("s-n" "s-p" "s-q" "H-q" "C-x C-c"))
  (unbind-key (kbd key)))

;;;; don't quit helpful on Esc
(evil-define-key 'motion help-mode-map (kbd "<escape>") nil)

(evil-define-key 'normal diff-mode-map "q" #'quit-window)

(global-set-key (kbd "C-x b") 'spacemacs-layouts/non-restricted-buffer-list-helm)
(global-set-key (kbd "C-x C-b") 'spacemacs-layouts/non-restricted-buffer-list-helm)

;;;; restore vanilla universal argument binding
(define-key evil-normal-state-map (kbd "C-u") 'universal-argument)

;;;; wrap `sp-up-sexp` "Move forward out of one level of parentheses", so it can be used in evil-lispy
(evil-lisp-state-enter-command sp-up-sexp)

(spacemacs/set-leader-keys
  "qq" nil                           ;; no unexpected exits
  "qQ" 'spacemacs/prompt-kill-emacs
  "s/" 'engine/search-google
  "jj" 'avy-goto-char-timer
  "xx" 'ispell-word
  "ja" 'beginning-of-defun
  "je" 'end-of-defun
  "kf" 'evil-lisp-state-sp-up-sexp
  "swg" 'ag/helm-google-suggest
  "ou" 'spacemacs/avy-open-url
  ;;;; add a page-break
  "iP" (kbd "i C-q C-l <RET><escape>"))

(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "h h" 'helpful-at-point)
(evil-define-key 'normal helpful-mode-map "q" 'quit-window)

(define-key evil-normal-state-map "Q" 'bury-buffer)
(define-key evil-normal-state-map (kbd "C-S-e") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-y") 'scroll-other-window-down)

;;;; ---------------
;;;; Smartparens
;;;; ---------------
(dolist (map (list evil-insert-state-map))
  (define-key map "\M-l" 'sp-slurp-hybrid-sexp)
  (define-key map "\M-h" 'sp-forward-barf-sexp)
  (define-key map "\M-L" 'sp-backward-slurp-sexp)
  (define-key map "\M-H" 'sp-backward-barf-sexp))

;;;; l and h are for navigating. even in magit
(with-eval-after-load 'magit
  (evil-define-key evil-magit-state magit-mode-map "l" 'evil-forward-char)
  (evil-define-key evil-magit-state magit-mode-map (kbd "M-l") 'magit-log-popup)
  (evil-define-key evil-magit-state magit-mode-map "h" 'evil-backward-char)
  (evil-define-key evil-magit-state magit-mode-map (kbd "M-h") 'magit-dispatch-popup))

(define-key Info-mode-map (kbd "H") 'Info-up)

;;;; -----------
;;;; Company
;;;; -----------
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-f") 'company-search-candidates))

;;; keybindings.el ends here
