;; Global keys
;;;; just disable nonsensical keys
(global-set-key (kbd "s-n") nil)
(global-set-key (kbd "s-p") nil)
(global-set-key (kbd "s-q") nil)
(global-set-key (kbd "C-x C-c") nil) ;; don't kill the frame unexpectedly
(evil-define-key 'motion help-mode-map (kbd "<escape>") nil)
(global-set-key (kbd "C-x b") 'spacemacs-layouts/non-restricted-buffer-list-helm)
(global-set-key (kbd "C-x C-b") 'spacemacs-layouts/non-restricted-buffer-list-helm)

(define-key evil-normal-state-map (kbd "C-u") 'universal-argument)

(spacemacs/set-leader-keys
  "qq" nil ;; no unexpected exits 
  "qQ" 'spacemacs/prompt-kill-emacs
  "s/" 'engine/search-google
  "jj" 'avy-goto-char-timer
  "xx" 'ispell-word)

(define-key evil-normal-state-map "Q" 'bury-buffer)
(define-key evil-normal-state-map (kbd "C-S-e") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-y") 'scroll-other-window-down)

(define-key evil-insert-state-map (kbd "s-l") 'sp-forward-symbol)
(define-key evil-insert-state-map (kbd "s-h") 'sp-backward-symbol)
(define-key evil-normal-state-map (kbd "s-l") 'sp-forward-symbol)
(define-key evil-normal-state-map (kbd "s-h") 'sp-backward-symbol)
(define-key evil-insert-state-map (kbd "s-L") 'sp-forward-sexp)
(define-key evil-insert-state-map (kbd "s-H") 'sp-backward-sexp)
(define-key evil-normal-state-map (kbd "s-L") 'sp-forward-sexp)
(define-key evil-normal-state-map (kbd "s-H") 'sp-backward-sexp)
(define-key evil-insert-state-map (kbd "s-j") 'lisp-state-next-closing-paren)
(define-key evil-insert-state-map (kbd "s-k") 'lisp-state-prev-opening-paren)
(define-key evil-normal-state-map (kbd "s-j") 'lisp-state-next-closing-paren)
(define-key evil-normal-state-map (kbd "s-k") 'lisp-state-prev-opening-paren)

;; Smartparens
(dolist (map (list evil-insert-state-map))
  (define-key map "\M-l" 'sp-slurp-hybrid-sexp)
  (define-key map "\M-h" 'sp-forward-barf-sexp)
  (define-key map "\M-L" 'sp-backward-slurp-sexp)
  (define-key map "\M-H" 'sp-backward-barf-sexp))

;; ---------------
;; web-mode
;; ---------------
(evil-define-key 'normal web-mode-map "zo" 'web-mode-fold-or-unfold)
(evil-define-key 'normal web-mode-map "zc" 'web-mode-fold-or-unfold)
(evil-leader/set-key-for-mode 'web-mode "mhh" 'web-mode-dom-xpath)
(evil-define-key 'normal web-mode-map "K" 'web-mode-dom-xpath)

;; ---------------
;; css-mode
;; ---------------
(evil-leader/set-key-for-mode 'css-mode "mhh" 'helm-css-scss)
(evil-define-key 'normal css-mode-map "K" 'helm-css-scss)

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

;; -----------
;; Inflections
;; -----------
(spacemacs/set-leader-keys
  "xi" 'string-inflection-all-cycle)

(evil-define-key 'normal info-mode-map "p" 'info-prev)

;; -----------
;; Company 
;; -----------
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-f") 'company-search-candidates))

(spacemacs/set-leader-keys
  "ip" (kbd "i C-q C-l <RET><escape>"))
