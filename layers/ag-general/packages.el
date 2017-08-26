(defconst ag-general-packages '(
                            ;; editorconfig
                            rainbow-mode
                            atomic-chrome
                            helm-pages))

(defun ag-general/init-editorconfig ()
  (use-package editorconfig
    :defer t
    :init
    (editorconfig-mode 1)))

(defun ag-general/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)))

(defun ag-general/init-atomic-chrome ()
  (use-package atomic-chrome
    :init
    (atomic-chrome-start-server)
    (define-key atomic-chrome-edit-mode-map (kbd "C-c C-c") 'atomic-chrome-close-current-buffer)
    :config
    (setq atomic-chrome-default-major-mode 'markdown-mode
          atomic-chrome-enable-bidirectional-edit t
          atomic-chrome-extension-type-list '(atomic-chrome))
    (add-hook 'atomic-chrome-edit-mode-hook #'ag/atomic-edit-start)
    (add-hook 'atomic-chrome-edit-done-hook #'ag/atomic-edit-done)))

(defun ag-general/init-helm-pages ()
  (use-package helm-pages
    :defer t))

(defun flycheck-mode-off () (flycheck-mode -1))
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode-off)

(with-eval-after-load 'ibuf-ext
  (setq
    ibuffer-old-time 8 ;; buffer considered old after that many hours
    ibuffer-group-buffers-by 'projects
    ibuffer-expert t
    ibuffer-show-empty-filter-groups nil)

  (define-ibuffer-filter unsaved-file-buffers
    "Toggle current view to buffers whose file is unsaved."
  (:description "file is unsaved")
  (ignore qualifier)
  (and (buffer-local-value 'buffer-file-name buf)
       (buffer-modified-p buf)))

  (define-ibuffer-filter file-buffers
      "Only show buffers backed by a file."
    (:description "file buffers")
    (ignore qualifier)
    (buffer-local-value 'buffer-file-name buf))

  (define-key ibuffer-mode-map (kbd "/ u") #'ibuffer-filter-by-unsaved-file-buffers)
  (define-key ibuffer-mode-map (kbd "/ F") #'ibuffer-filter-by-file-buffers))
