;;; packages.el --- ag-general layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-general-packages '(
                                ;; adding magithub explicitly here, until this issue is fixed:
                                ;; see: https://github.com/syl20bnr/spacemacs/issues/9288 and:
                                ;; https://github.com/magit/magit/issues/3154#issuecomment-325648623
                                ;; and wait when they enable this:
                                ;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+source-control/github/packages.el#L19
                                ;; magithub
                                helpful
                                rainbow-mode
                                ;; atomic-chrome
                                helm-pages
                                evil-mc
                                edit-indirect
                                engine-mode
                                ))

;; (defun ag-general/init-magithub ()
;;   (use-package magithub
;;     :after magit
;;     :init
;;     (setq magithub-dir (concat spacemacs-cache-directory "magithub/"))
;;     :config
;;     (progn
;;       (magithub-feature-autoinject t)
;;       (define-key magit-status-mode-map "H" #'magithub-dispatch-popup))))

(defun ag-general/init-helpful ()
  (use-package helpful
    :config
    (unbind-key "h" help-map)
    (bind-key "hh" 'helpful-symbol help-map)
    (bind-key "ha" 'helpful-at-point help-map)
    (spacemacs/set-leader-keys "hdd" #'helpful-symbol)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "h h" 'helpful-at-point)
    (evil-define-key 'normal helpful-mode-map "q" 'quit-window)))

(defun ag-general/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)
    ;; remove rectangles from around the colors
    (setq css-fontify-colors nil)))

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

(with-eval-after-load 'flycheck
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (defun flycheck-mode-off () (flycheck-mode -1))
    (add-hook 'emacs-lisp-mode-hook #'flycheck-mode-off)))

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

;; Taken from the discussion here: https://github.com/syl20bnr/spacemacs/issues/2669
;; TODO: remove this when official method is implemented in Spacmacs
(defun ag-general/init-evil-mc ()
  (use-package evil-mc
    :init
    (unbind-key "C-t")
    :config
    (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
    (add-hook 'text-mode-hook 'turn-on-evil-mc-mode)

    (defun evil--mc-make-cursor-at-col (startcol _endcol orig-line)
      (move-to-column startcol)
      (unless (= (line-number-at-pos) orig-line)
        (evil-mc-make-cursor-here)))
    (defun evil-mc-make-vertical-cursors (beg end)
      (interactive (list (region-beginning) (region-end)))
      (evil-mc-pause-cursors)
      (apply-on-rectangle #'evil--mc-make-cursor-at-col
                          beg end (line-number-at-pos (point)))
      (evil-mc-resume-cursors)
      (evil-normal-state)
      (move-to-column (evil-mc-column-number (if (> end beg)
                                                 beg
                                               end))))))
(defun ag-general/init-edit-indirect ()
  (use-package edit-indirect
    :config
    (defun ag/edit-indirect-guess-mode (parent-buffer beg end)
      (let ((major (with-current-buffer parent-buffer major-mode)))
        (cond ((eq major 'clojurescript-mode) (org-mode))
              (t (funcall major)))))
    (setq edit-indirect-guess-mode-function 'ag/edit-indirect-guess-mode)))

(defun ag-general/post-init-engine-mode ()
  (defcustom engine-mode/github-mode->lang
    '(("clojurescript" . "Clojure")
      ("clojure" . "Clojure")
      ("clojurec" . "Clojure")
      ("emacs-lisp" . "Emacs Lisp"))
    "Associates current mode with a language in Github terms"
    :type 'alist
    :group 'engine)

  (defun engine/search-github-with-lang ()
    "Search on Github with attempt of detecting language associated with current-buffer's mode"
    (interactive)
    (let* ((mode-name (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
           (lang (cdr (assoc mode-name engine-mode/github-mode->lang)))
           (lang-term (if lang (concat "language:\"" lang "\" ") ""))
           (current-word (or (thing-at-point 'symbol) ""))
           (search-term (read-string "Search Github: " (concat lang-term current-word))))
      (engine/search-github search-term)))

  (spacemacs/set-leader-keys "g/" #'engine/search-github-with-lang))

;;; packages.el ends here
