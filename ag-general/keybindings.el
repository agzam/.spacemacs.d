;; Global keys
(global-set-key (kbd "s-n") nil)
(global-set-key (kbd "s-p") nil)
(global-set-key (kbd "s-q") nil)
(global-set-key (kbd "C-x b") 'spacemacs-layouts/non-restricted-buffer-list-helm)
(global-set-key (kbd "C-x C-b") 'spacemacs-layouts/non-restricted-buffer-list-helm)

;; (bind-key* "<C-u>" 'universal-argument)
;; (global-set-key (kbd "C-u") 'universal-argument)
(define-key evil-normal-state-map (kbd "C-u") 'universal-argument)

(spacemacs/set-leader-keys
  "qq" nil ;; no quitting suddenly
  "qQ" 'spacemacs/prompt-kill-emacs) 

(define-key evil-normal-state-map "Q" 'bury-buffer)

(define-key evil-normal-state-map (kbd "C-S-e") 'scroll-other-window)
(define-key evil-normal-state-map (kbd "C-S-y") 'scroll-other-window-down)
;; (define-key evil-normal-state-map "\C-k" 'er/expand-region)
;; (define-key evil-normal-state-map "\C-j" 'er/contract-region)

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

;; Clojure
(with-eval-after-load 'cider
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode
               cider-repl-mode))
    (spacemacs/set-leader-keys-for-major-mode m
      ";" 'cljr-toggle-ignore-form
      "h h" 'helm-clojuredocs-at-point
      "'" 'cider-switch-to-repl-buffer)))

;; (defun ag/simple-parens (mode)
;;   (evil-define-key 'insert mode "9" (lambda () (interactive) (paredit-open-parenthesis)))
;;   (evil-define-key 'insert mode "0" (lambda () (interactive) (spacemacs/smart-closing-parenthesis)))
;;   (evil-define-key 'insert mode "(" (lambda () (interactive) (insert "9")))
;;   (evil-define-key 'insert mode ")" (lambda () (interactive) (insert "0"))))

;; (add-hook 'clojure-mode-hook (lambda () (ag/simple-parens clojure-mode-map)))
;; (add-hook 'cider-repl-mode-hook (lambda () (ag/simple-parens cider-repl-mode-map)))
;; (add-hook 'clojurescript-mode-hook (lambda () (ag/simple-parens clojurescript-mode-map)))
;; (add-hook 'emacs-lisp-mode-hook (lambda () (ag/simple-parens emacs-lisp-mode-map)))

;; ;; Clojure cider REPL eval
;; (evil-define-key 'normal clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
;; (evil-define-key 'insert clojure-mode-map (kbd "<C-return>") 'cider-eval-last-sexp)
;; (evil-define-key 'visual clojure-mode-map (kbd "<C-return>") 'cider-eval-region)

;; (with-eval-after-load 'doc-view (evilified-state-evilify doc-view-mode doc-view-mode-map
;;                                   "l" 'doc-view-scroll-up-or-next-page
;;                                   "h" 'doc-view-scroll-down-or-previous-page))

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
(evil-define-key 'normal css-mode-map "zc" 'css-contract-statement)
(evil-define-key 'normal css-mode-map "zo" 'css-expand-statement)
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
;; Direx
;; -----------
(spacemacs/set-leader-keys
  "ft" 'direx:jump-to-directory
  "pt" 'direx-project:jump-to-project-root)

(evil-define-key 'normal direx:direx-mode-map "r" 'direx:refresh-whole-tree)

;; -----------
;; Inflections
;; -----------
(spacemacs/set-leader-keys
  "xi" 'string-inflection-all-cycle)

(evil-define-key 'normal info-mode-map "p" 'info-prev)
