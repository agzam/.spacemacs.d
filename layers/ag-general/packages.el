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
                                edit-indirect
                                engine-mode
                                fennel-mode
                                ,(when (eq system-type 'darwin)
                                   '(spacehammer :location "~/.hammerspoon"))
                                ;; (jira :location local)
                                lsp-mode
                                all-the-icons-ivy-rich
                                company-tabnine
                                (evilify-edebug :location local)
                                company-posframe
                                ivy-posframe
                                which-key-posframe
                                ;; grip-mode
                                ;; undo-fu
                                expand-region
                                doom-modeline
                                flycheck-posframe
                                marginalia))

(defun ag-general/init-doom-modeline ()
  (use-package doom-modeline
    :defer t
    :init
    (doom-modeline-mode)
    (doom-modeline-def-modeline
      'agcustom
      '(bar window-number workspace-name persp-name buffer-info matches word-count parrot selection-info)
      '(misc-info battery grip irc mu4e debug repl lsp major-mode process checker buffer-position))

    (defun setup-custom-doom-modeline ()
      (doom-modeline-set-modeline 'agcustom))

    (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline 90)
    (add-hook 'find-file-hook 'setup-custom-doom-modeline 90)

    (setq doom-modeline-buffer-encoding nil
          doom-modeline-buffer-file-name-style 'relative-from-project
          doom-modeline-buffer-modification-icon nil
          doom-modeline-buffer-state-icon nil
          doom-modeline-icon (display-graphic-p)
          doom-modeline-major-mode-color-icon nil
          doom-modeline-major-mode-icon nil
          doom-modeline-modal-icon t
          doom-modeline-mu4e nil
          doom-modeline-persp-icon nil
          inhibit-compacting-font-caches t
          doom-modeline-height 1
          doom-modeline-bar-width 1)))

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
        ;; (spacehammer/fix-frame)
        )

      (define-advice spacemacs/zoom-frm-transient-state/spacemacs/zoom-frm-out
          (:around (fn) fix-frame-after-zoom-frm-out)
        (reinforce-frame-full-width-height)
        (funcall fn)
        ;; (spacehammer/fix-frame)
        ))))

(defun ag-general/init-jira ()
  (use-package jira
    ;; :demand t
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
          lsp-diagnostics-provider :flycheck
          lsp-enable-file-watchers nil
          lsp-modeline--enable-code-actions nil
          lsp-headerline-breadcrumb-enable nil
          lsp-enable-completion-at-point t)
    (add-hook
     'lsp-after-open-hook
     (lambda()
       (lsp-modeline-code-actions-mode -1)
       (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
         "hh" nil
         "," #'lsp-ui-doc-glance)))))

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

(defun ag-general/init-evilify-edebug ()
  (use-package evilify-edebug
    :commands (edebug)
    :config
    (evilify-edebug-setup)))

(defun ag-general/init-company-posframe ()
  (use-package company-posframe
    :demand t
    :init
    (setq company-posframe-quickhelp-delay nil)
    :bind (:map company-active-map
                ("C-h" . (lambda () (interactive) (company-posframe-quickhelp-show)))
                ("C-c C-d". company-show-doc-buffer)
                ("C-n" . company-select-next)
                ("C-p" . company-select-previous)
                ("C-c C-l". company-show-location)
                :map company-posframe-active-map
                ("C-c h" . company-posframe-quickhelp-toggle))
    :config
    (company-posframe-mode 1)))

(defun ag-general/init-ivy-posframe ()
  (use-package ivy-posframe
    :after (ivy posframe)
    :config
    (add-hook 'spacemacs-post-theme-change-hook 'ag/ivy-postframe--set-parameters)

    (setq ivy-posframe-display-functions-alist
          '((t . ag/ivy-posframe-display-at-frame-bottom-left)))

    (setq posframe-arghandler #'ag/posframe-arghandler)

    (ivy-posframe-mode 1)))

(defun ag-general/init-which-key-posframe ()
  (use-package which-key-posframe
    :config
    ;; (setq which-key-posframe-parameters
    ;;       '((undecorated . t)
    ;;         (internal-border-width . 1)))
    ;; (defun posframe-poshandler-frame-bottom-right-corner (info)
    ;;   (cons (- (plist-get info :parent-frame-width)
    ;;            (+ (plist-get info :posframe-width) 10))
    ;;         (- (plist-get info :parent-frame-height)
    ;;            (+ (plist-get info :posframe-height) 85))))
    ;; (setq which-key-posframe-poshandler #'posframe-poshandler-frame-bottom-right-corner
    ;;       which-key-min-display-lines 15)
    ;; (which-key-posframe-mode 1)
    ))

(defun ag-general/init-grip-mode ()
  (use-package grip-mode
    :config
    (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
      "v" 'grip-mode)
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "v" 'grip-mode)))

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
(add-hook 'shell-mode-hook 'spacemacs/toggle-truncate-lines-on)
(add-hook 'shell-pop-out-hook 'comint-write-input-ring)
(add-hook 'shell-pop-process-exit-hook 'comint-write-input-ring)


(setq eshell-history-file-name (getenv "HISTFILE")
      eshell-aliases-file "~/.spacemacs.d/.eshell.aliases")

;;;;;;;;;;;;;;;;;;;;;;;
;; expand region fix ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; Expand region for whatever reason ignores lines when expanding, it should
;; start expanding from a word, then to a line, then to a paragraph, and so
;; on. But default implementation ignores the line expansion.

(defun ag-general/post-init-expand-region ()
  (setq er/try-expand-list
        '(er/mark-word
          mark-between
          er/mark-symbol
          er/mark-symbol-with-prefix
          er/mark-line
          er/mark-next-accessor
          er/mark-method-call
          er/mark-inside-quotes
          er/mark-outside-quotes
          er/mark-inside-pairs
          er/mark-outside-pairs
          er/mark-comment
          er/mark-url
          er/mark-email
          er/mark-defun))

  (defun er/add-org-mode-expansions* ()
    "Adds org-specific expansions for buffers in org-mode"
    (set (make-local-variable 'er/try-expand-list)
         '(er/mark-word
           mark-between
           er/mark-symbol
           er/mark-symbol-with-prefix
           er/mark-inside-pairs
           er/mark-line
           er/mark-org-element
           er/mark-org-element-parent
           er/mark-org-code-block
           er/mark-org-parent))
    (set (make-local-variable 'er/save-mode-excursion)
         #'er/save-org-mode-excursion))

  (with-eval-after-load 'expand-region
    (remove-hook 'org-mode-hook 'er/add-org-mode-expansions)
    (er/enable-mode-expansions 'org-mode #'er/add-org-mode-expansions*)))

(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map flycheck-keymap-prefix nil))

(defun ag-general/init-flycheck-posframe ()
  (use-package flycheck-posframe
    :hook (flycheck-mode . flycheck-posframe-mode)))

(defun ag-general/init-marginalia ()
  (use-package marginalia
    :bind (:map ivy-minibuffer-map ("M-C" . marginalia-cycle)
           (:map minibuffer-local-map ("M-C" . marginalia-cycle)))
   :init
   (marginalia-mode)
   :custom
   (marginalia-annotators
    '(marginalia-annotators-heavy
      marginalia-annotators-light))))

(with-eval-after-load 'terminal-here
  (setq terminal-here-terminal-command
        (lambda (dir)
          (cond
           ((eq system-type 'darwin)
            (list "open" "-a" "iTerm" dir))))))

(with-eval-after-load 'ivy
  (defun ivy--recenter-after-press ()
    (with-ivy-window (recenter)))

  (advice-add 'ivy-occur-press :after 'ivy--recenter-after-press))

;;; packages.el ends here
