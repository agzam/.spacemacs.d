;; -*-lexical-binding: t -*-

(defconst ag-lsp-packages
  `(lsp-mode))

(defun ag-lsp/post-init-lsp-mode ()
  (with-eval-after-load 'lsp-mode
    (setq lsp-after-open-hook nil
          lsp-eldoc-enable-hover t
          lsp-eldoc-render-all nil
          lsp-modeline-diagnostics-enable nil
          lsp-diagnostics-provider :flycheck
          lsp-enable-file-watchers nil
          lsp-modeline--enable-code-actions nil
          lsp-headerline-breadcrumb-enable nil
          lsp-enable-completion-at-point t
          lsp-enable-symbol-highlighting t
          lsp-enable-imenu nil
          lsp-treemacs-errors-position-params '((side . right))
          lsp-treemacs-sync-mode nil

          lsp-ui-sideline-enable nil
          lsp-ui-doc-enable nil
          lsp-ui-doc-position 'top
          lsp-completion-enable nil
          lsp-modeline-code-actions-enable nil

          lsp-semantic-tokens-enable nil
          lsp-lens-enable nil

          ivy-xref-use-file-path t)
    (add-hook
     'lsp-after-open-hook
     (lambda()
       (lsp-modeline-code-actions-mode -1)
       (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
         "hh" nil
         "gD" #'xref-find-definitions-other-window
         "," #'lsp-ui-doc-glance
         "ge" #'lsp-ui-flycheck-list+)))))
