;; Global keys
;;;; just disable nonsensical keys
(global-set-key (kbd "s-n") nil)
(global-set-key (kbd "s-p") nil)
(global-set-key (kbd "s-q") nil)
(global-set-key (kbd "H-q") nil)
(global-set-key (kbd "C-x C-c") nil) ;; don't kill the frame unexpectedly
(evil-define-key 'motion help-mode-map (kbd "<escape>") nil)
(evil-define-key 'normal diff-mode-map "q" #'quit-window)
(global-set-key (kbd "C-x b") 'spacemacs-layouts/non-restricted-buffer-list-helm)
(global-set-key (kbd "C-x C-b") 'spacemacs-layouts/non-restricted-buffer-list-helm)

(define-key evil-normal-state-map (kbd "C-u") 'universal-argument)

(evil-lisp-state-enter-command sp-up-sexp)

(spacemacs/set-leader-keys
  "qq" nil ;; no unexpected exits
  "qQ" 'spacemacs/prompt-kill-emacs
  "s/" 'engine/search-google
  "jj" 'avy-goto-char-timer
  "xx" 'ispell-word
  "ja" 'beginning-of-defun
  "je" 'end-of-defun
  "kf" 'evil-lisp-state-sp-up-sexp)

(define-key evil-normal-state-map "Q" 'bury-buffer)
(define-key evil-normal-state-map (kbd "C-S-e") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-y") 'scroll-other-window-down)

;; (define-key evil-insert-state-map (kbd "s-l") 'sp-forward-symbol)
;; (define-key evil-insert-state-map (kbd "s-h") 'sp-backward-symbol)
;; (define-key evil-normal-state-map (kbd "s-l") 'sp-forward-symbol)
;; (define-key evil-normal-state-map (kbd "s-h") 'sp-backward-symbol)
;; (define-key evil-insert-state-map (kbd "s-L") 'sp-forward-sexp)
;; (define-key evil-insert-state-map (kbd "s-H") 'sp-backward-sexp)
;; (define-key evil-normal-state-map (kbd "s-L") 'sp-forward-sexp)
;; (define-key evil-normal-state-map (kbd "s-H") 'sp-backward-sexp)
;; (define-key evil-insert-state-map (kbd "s-j") 'lisp-state-next-closing-paren)
;; (define-key evil-insert-state-map (kbd "s-k") 'lisp-state-prev-opening-paren)
;; (define-key evil-normal-state-map (kbd "s-j") 'lisp-state-next-closing-paren)
;; (define-key evil-normal-state-map (kbd "s-k") 'lisp-state-prev-opening-paren)

;; Smartparens
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

;; ------------
;; Javascript mode
;; ------------
(evil-define-key 'normal js2-mode-map "zc" 'js2-mode-hide-element)
(evil-define-key 'normal js2-mode-map "zo" 'js2-mode-show-element)
(evil-leader/set-key-for-mode 'js2-mode "mhh" 'tern-get-docs)

;; ---------------
;; Misc
;; ---------------
(spacemacs/set-leader-keys
  "swg" 'ag/helm-google-suggest
  "ou" 'spacemacs/avy-open-url)

(evil-define-key 'normal magit-diff-mode-map "l" #'evil-forward-char)
(evil-define-key 'normal magit-diff-mode-map "h" #'evil-backward-char)

;; -----------
;; Inflections
;; -----------
;; (spacemacs/set-leader-keys
;;   "xi" 'string-inflection-all-cycle)

(evil-define-key 'normal info-mode-map "p" 'info-prev)

;; -----------
;; Company
;; -----------
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-f") 'company-search-candidates))

;; add a page-break
(spacemacs/set-leader-keys
  "ip" (kbd "i C-q C-l <RET><escape>"))
