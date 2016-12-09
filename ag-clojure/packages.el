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

(setq ag-clojure-packages '())

(defconst ag-clojure-packages
  '(
    ac-cider
    flycheck-clojure
    clojure-mode-extra-font-locking
    helm-clojuredocs
    clojars
    clojure-cheatsheet))

(defun ag-clojure/init-ac-cider ())
(defun ag-clojure/init-flycheck-clojure ())
(defun ag-clojure/init-clojure-mode-extra-font-locking ())
(defun ag-clojure/init-helm-clojuredocs ())
(defun ag-clojure/init-clojars ())
(defun ag-clojure/init-clojure-cheatsheet ())

(with-eval-after-load 'clojure-mode
  (setq clojure-enable-fancify-symbols nil
        clojure-indent-style :align-arguments 
        clojure-align-forms-automatically t
        cider-overlays-use-font-lock nil
        cider-repl-use-clojure-font-lock nil
        cider-font-lock-dynamically nil
        nrepl-log-messages nil)
  (add-hook 'clojure-mode 'flyspell-prog-mode)

  ;; annoying Java Cup icon - no longer will bother you
  (setenv "JAVA_TOOL_OPTIONS" "-Dapple.awt.UIElement=true"))
