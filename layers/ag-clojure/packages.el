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
    lsp-mode))

(defun ag-clojure/init-ac-cider ())
(defun ag-clojure/init-clojure-mode-extra-font-locking ())
(defun ag-clojure/init-helm-clojuredocs ())
(defun ag-clojure/init-clojars ())
(defun ag-clojure/init-helm-cider ())
(defun ag-clojure/init-cider-hydra ())

(defun ag-clojure/post-init-lsp-mode ()
  (with-eval-after-load 'lsp-mode
    ;; (setq lsp-clojure-server-command `("java" "-jar" ,(concat (getenv "HOME") "/.clojure/clj-kondo-lsp-server.jar")))
    (dolist (m '(clojure-mode
                 clojurec-mode
                 clojurescript-mode
                 clojurex-mode))
      (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))

    (define-key lsp-mode-map (kbd "TAB") #'company-indent-or-complete-common))

  (spacemacs|add-company-backends
    :backends company-lsp
    :modes '(clojure-mode clojurescript-mode clojurec-mode)
    :variables company-minimum-prefix-length 2
    :append-hooks nil
    :call-hooks t)

  (setq lsp-enable-indentation nil)

  (defun lsp-init ()
    (run-at-time "1 sec" nil #'lsp))

  (add-hook 'clojure-mode-hook #'lsp-init)
  (add-hook 'clojurec-mode-hook #'lsp-init)
  (add-hook 'clojurescript-mode-hook #'lsp-init))

(with-eval-after-load 'clojure-mode
  (setq clojure-enable-fancify-symbols nil
        ;; clojure-indent-style :align-arguments
        clojure-align-forms-automatically t
        cljr-favor-prefix-notation nil
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

  (add-to-list 'ivy-re-builders-alist '(cider-find-ns . ivy--regex-fuzzy))

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

  (with-eval-after-load 'clojure-mode
    (dolist (c (string-to-list ":_-?!*"))
      (modify-syntax-entry c "w" clojure-mode-syntax-table)
      (modify-syntax-entry c "w" clojurescript-mode-syntax-table)))

  ;; compojure's fucked indentation
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (OPTIONS 2)
    (PATCH 2)
    (rfn 2)
    (let-routes 1)
    (context 2)
    (clojure.test/async 1)
    (promesa.core/alet 1)
    )

  )

(add-hook 'clojurescript-mode-hook #'add-reframe-regs-to-imenu)
;;; packages.el ends here
