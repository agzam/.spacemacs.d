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

(defconst ag-general-packages `(helpful
                                rainbow-mode
                                ;; evil-mc
                                edit-indirect
                                engine-mode
                                fennel-mode
                                ,(when (eq system-type 'darwin)
                                   '(spacehammer :location "~/.hammerspoon"))
                                (jira :location local)
                                lsp-mode
                                all-the-icons-ivy-rich
                                company-tabnine))

(defun ag-general/init-helpful ()
  (use-package helpful
    :config
    (unbind-key "h" help-map)
    (bind-key "hh" 'helpful-symbol help-map)
    (bind-key "ha" 'helpful-at-point help-map)
    (global-set-key (kbd "C-h k") #'helpful-key)
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

(with-eval-after-load 'flycheck
  (progn
    (add-hook 'prog-mode-hook 'flycheck-mode)
    (defun flycheck-mode-off () (flycheck-mode -1))
    (add-hook 'emacs-lisp-mode-hook #'flycheck-mode-off)))

(with-eval-after-load 'ibuf-ext
  (setq
   ibuffer-old-time 8 ; buffer considered old after that many hours
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
  (let ((custom-search-engines
         '((maven :name "Maven Central"
                  :url "https://search.maven.org/search?q=%s")
           (npm :name "npmjs"
                :url "https://www.npmjs.com/search?q=%s"))))
    (dolist (se custom-search-engines)
      (push se search-engine-alist)
      (autoload (intern (format "engine/search-%S" (car se)))
        "engine-mode" nil 'interactive)))

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

(defun ag-general/init-fennel-mode ()
  (use-package fennel-mode
    :init
    (autoload 'clojure-align "clojure-mode" nil t)
    :config
    (spacemacs/set-leader-keys-for-major-mode
      'fennel-mode
      "fl"
      'clojure-align)))

(defun ag-general/init-spacehammer ()
  (use-package spacehammer
    :demand t
    :config

    (progn
      (defun on-spacehammer-edit-with-emacs (buffer-name pid title)
        (with-current-buffer (get-buffer buffer-name)
          (spacemacs/evil-search-clear-highlight)
          (spacemacs/toggle-visual-line-navigation-on)
          (markdown-mode)
          (variable-pitch-mode)
          (evil-insert 1)))

      (add-hook 'spacehammer/edit-with-emacs-hook 'on-spacehammer-edit-with-emacs)

      (spacemacs/transient-state-register-add-bindings 'zoom-frm
        '(("h" (spacehammer/move-frame-one-display "West"))
          ("l" (spacehammer/move-frame-one-display "East"))
          ("n" (spacehammer/move-frame-one-display "North"))
          ("p" (spacehammer/move-frame-one-display "South"))))

      (define-advice spacemacs/zoom-frm-transient-state/spacemacs/zoom-frm-in
          (:around (fn) fix-frame-after-zoom-frm-in)
        (reinforce-frame-full-width-height)
        (funcall fn)
        (spacehammer/fix-frame))

      (define-advice spacemacs/zoom-frm-transient-state/spacemacs/zoom-frm-out
          (:around (fn) fix-frame-after-zoom-frm-out)
        (reinforce-frame-full-width-height)
        (funcall fn)
        (spacehammer/fix-frame)))))

(defun ag-general/init-jira ()
  (use-package jira
    :demand t
    :config
    (setq jira-base-url "https://jira.dividendsolar.com")
    (global-set-key (kbd "M-o M-j") #'convert-to-jira-link)))

(defun spacemacs//setup-lsp-jump-handler* (&rest modes)
  "Set jump handler for LSP with the given MODE."
  (dolist (m modes)
    (add-to-list (intern (format "spacemacs-jump-handlers-%S" m))
                 '(xref-find-definitions :async t))))

(defun ag-general/post-init-lsp-mode ()
  (with-eval-after-load 'lsp-mode
    (setq lsp-after-open-hook nil
          lsp-eldoc-enable-hover nil
          lsp-diagnostic-package :none
          lsp-enable-file-watchers nil)
    (add-hook
     'lsp-after-open-hook
     (lambda()
       (spacemacs//lsp-declare-prefixes-for-mode major-mode)
       (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
         "hh" nil)))))

(defun ag-general/init-all-the-icons-ivy-rich ()
  (use-package all-the-icons-ivy-rich
    :ensure t
    :init
    (all-the-icons-ivy-rich-mode 1)
    (setq all-the-icons-ivy-rich-icon-size 0.8)))

(defun ag-general/init-company-tabnine ()
  (use-package company-tabnine
    :ensure t
    :config
    (spacemacs|add-company-backends
      :backends company-tabnine
      :modes sql-mode sql-interactive-mode)))


(when (eq system-type 'gnu/linux)
  (defun on-stump-edit-with-emacs (buffer-name window-title window-class window-id)
    (delete-other-windows)
    (with-current-buffer (get-buffer buffer-name)
      (spacemacs/evil-search-clear-highlight)
      (spacemacs/toggle-visual-line-navigation-on)
      (markdown-mode)
      (evil-insert)
      (variable-pitch-mode)
      (flyspell-mode)))

  (add-hook 'stump/edit-with-emacs-hook 'on-stump-edit-with-emacs))

(defun comint-write-history-on-exit (process event)
  (comint-write-input-ring))

(defun turn-on-comint-history ()
  (when-let ((proc (get-buffer-process (current-buffer))))
    (set-process-sentinel proc 'comint-write-history-on-exit)
    (setq comint-input-ring-file-name (or (getenv "HISTFILE") "~/.zsh_history"))
    (comint-read-input-ring 'silent)))

(add-hook 'shell-mode-hook 'turn-on-comint-history)
(add-hook 'shell-pop-out-hook 'comint-write-input-ring)
(add-hook 'shell-pop-process-exit-hook 'comint-write-input-ring)


(setq eshell-history-file-name (getenv "HISTFILE")
      eshell-aliases-file "~/.spacemacs.d/.eshell.aliases")

;; (remove-hook 'kill-buffer-hook 'comint-write-history-on-exit)

;;; packages.el ends here
