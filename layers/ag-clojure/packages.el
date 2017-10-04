;;; packages.el --- ag-clojure layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <ag.ibragimov@C02MT2ZDFH05>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-clojure-packages
  '(ac-cider
    clojure-mode-extra-font-locking
    helm-clojuredocs
    clojars
    clojure-cheatsheet
    helm-cider
    flycheck-joker))

(defun ag-clojure/init-ac-cider ())
(defun ag-clojure/init-clojure-mode-extra-font-locking ())
(defun ag-clojure/init-helm-clojuredocs ())
(defun ag-clojure/init-clojars ())
(defun ag-clojure/init-clojure-cheatsheet ())
(defun ag-clojure/init-helm-cider ())
(defun ag-clojure/init-cider-hydra ())
(defun ag-clojure/init-flycheck-joker ()
  (require 'flycheck-joker))

;; (defun ag-clojure/add-jump-to (l) (add-to-list l '(dumb-jump-go)))

(with-eval-after-load 'clojure-mode
  (setq clojure-enable-fancify-symbols nil
        clojure-indent-style :align-arguments
        clojure-align-forms-automatically t
        cider-overlays-use-font-lock nil
        cider-repl-use-clojure-font-lock nil
        cider-font-lock-dynamically nil
        nrepl-log-messages nil
        clojure-align-binding-forms '("binding" "loop" "doseq" "for" "with-open" "with-local-vars" "with-redefs"))

  ;; (add-hook 'clojure-mode #'flyspell-prog-mode)

  ;; (add-hook 'clojure-mode-hook (lambda () (add-to-list 'spacemacs-jump-handlers-clojure-mode 'dumb-jump-go)))
  ;; (add-hook 'clojurescript-mode-hook (lambda () (add-to-list 'spacemacs-jump-handlers-clojurescript-mode 'dumb-jump-go)))
  ;; (add-hook 'clojurec-mode-hook (lambda () (add-to-list 'spacemacs-jump-handlers-clojurec-mode 'dumb-jump-go)))
  ;; (add-hook 'cider-repl-mode-hook (lambda () (add-to-list 'spacemacs-jump-handlers-cider-repl-mode 'dumb-jump-go)))

  ;; annoying Java Cup icon - no longer will bother you
  (setenv "JAVA_TOOL_OPTIONS" "-Dapple.awt.UIElement=true"))

;; (with-eval-after-load 'cider (setq cider-boot-parameters "dev"))
