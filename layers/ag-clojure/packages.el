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
  '(clojure-mode-extra-font-locking
    helm-clojuredocs
    clojars
    helm-cider
    flycheck-joker
    flycheck-clj-kondo))

(defun ag-clojure/init-ac-cider ())
(defun ag-clojure/init-clojure-mode-extra-font-locking ())
(defun ag-clojure/init-helm-clojuredocs ())
(defun ag-clojure/init-clojars ())
(defun ag-clojure/init-helm-cider ())
(defun ag-clojure/init-cider-hydra ())
(defun ag-clojure/init-flycheck-joker ())

(defun ag-clojure/init-flycheck-clj-kondo ()
  (require 'flycheck-joker)
  (require 'flycheck-clj-kondo)
  (dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
    (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
  (dolist (checkers '((clj-kondo-clj . clojure-joker)
                      (clj-kondo-cljs . clojurescript-joker)
                      (clj-kondo-cljc . clojure-joker)
                      (clj-kondo-edn . edn-joker)))
    (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))

(with-eval-after-load 'clojure-mode
  (setq clojure-enable-fancify-symbols nil
        ;; clojure-indent-style :align-arguments
        clojure-align-forms-automatically nil
        cider-overlays-use-font-lock nil
        cider-repl-use-clojure-font-lock nil
        cider-font-lock-dynamically nil
        cider-repl-display-in-current-window nil
        nrepl-log-messages nil
        cider-comment-prefix  " \n;; => "
        ;; clojure-align-binding-forms '("binding" "loop" "doseq" "for" "with-open" "with-local-vars" "with-redefs")

        ;; make * operator (spacemacs/enter-ahs-forward) to recognize symbols like foo/bar
        ahs-include "^\\_<\\(?:\\s_\\|\\sw\\)+\\_>$"
        )

  ;; (dolist (form '(re-frame.core/reg-sub
  ;;                 re-frame.core/reg-fx
  ;;                 re-frame.core/reg-sub
  ;;                 re-frame.core/reg-event-fx
  ;;                 re-frame.core/reg-event-db
  ;;                 rf/reg-sub
  ;;                 rf/reg-event-fx
  ;;                 rf/reg-event-db
  ;;                 clojure.spec.alpha/fdef
  ;;                 cljs.spec.alpha/fdef
  ;;                 prop/for-all))
  ;;   (put-clojure-indent form 1))

  ;; annoying Java Cup icon - no longer will bother you
  (setenv "JAVA_TOOL_OPTIONS" "-Dapple.awt.UIElement=true")

  (defun before-eval-print-advice (old-f &rest args)
    (if (thing-at-point 'list)
        (end-of-thing 'list)
      (end-of-thing 'symbol))
    (insert "\n")
    (apply old-f args)
    (run-at-time "0.05 sec" nil
                 (lambda ()
                   (backward-sexp)
                   (cider-format-edn-last-sexp))))

  (advice-add 'cider-eval-print-last-sexp :around #'before-eval-print-advice)

  (defun clojure--hyphens-in-words () (modify-syntax-entry ?- "w"))
  (remove-hook 'clojure-mode-hook #'clojure--hyphens-in-words)
  (remove-hook 'clojurescript-mode-hook #'clojure--hyphens-in-words)
  (remove-hook 'clojurec-mode-hook #'clojure--hyphens-in-words)
  )

(add-hook 'clojurescript-mode-hook #'add-reframe-regs-to-imenu)
;;; packages.el ends here
