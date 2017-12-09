;;; packages.el --- ag-clojure layer packages.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
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
        clojure-align-forms-automatically nil
        cider-overlays-use-font-lock nil
        cider-repl-use-clojure-font-lock nil
        cider-font-lock-dynamically nil
        nrepl-log-messages nil
        clojure-align-binding-forms '("binding" "loop" "doseq" "for" "with-open" "with-local-vars" "with-redefs"))

  (dolist (form '(re-frame.core/reg-sub
                  re-frame.core/reg-fx
                  re-frame.core/reg-sub
                  re-frame.core/reg-event-fx
                  re-frame.core/reg-event-db))
    (put-clojure-indent form 1))

  ;; (add-hook 'clojure-mode #'flyspell-prog-mode)

  (add-hook 'clojure-mode-hook #'spacemacs//init-jump-handlers-clojure-mode)
  (add-hook 'clojurescript-mode-hook #'spacemacs//init-jump-handlers-clojurescript-mode)
  (add-hook 'clojurec-mode-hook #'spacemacs//init-jump-handlers-clojurec-mode)
  (add-hook 'cider-repl-mode-hook #'spacemacs//init-jump-handlers-cider-repl-mode)

  ;; annoying Java Cup icon - no longer will bother you
  (setenv "JAVA_TOOL_OPTIONS" "-Dapple.awt.UIElement=true")

  (dolist (x '(spacemacs-jump-handlers-clojure-mode
               spacemacs-jump-handlers-clojurec-mode
               spacemacs-jump-handlers-clojurescript-mode
               spacemacs-jump-handlers-clojurex-mode
               spacemacs-jump-handlers-cider-repl-mode))
    (set x '(clj-find-var))))

;; (with-eval-after-load 'cider (setq cider-boot-parameters "dev"))

;;; packages.el ends here
