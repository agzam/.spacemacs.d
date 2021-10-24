;; -*- lexical-binding: t -*-
;;; packages.el --- ag-general layer packages file for Spacemacs.

(defconst ag-general-packages
  `(rainbow-mode
    edit-indirect
    engine-mode
    fennel-mode
    ,(when (eq system-type 'darwin)
       '(spacehammer :location "~/.hammerspoon"))
    ;; (jira :location local)
    company-tabnine
    company-posframe
    ivy-posframe
    which-key-posframe
    ;; grip-mode
    ;; undo-fu
    expand-region
    doom-modeline
    flycheck-posframe
    marginalia
    ivy-prescient
    company-prescient))

(defun ag-general/init-doom-modeline ()
  (use-package doom-modeline
    :init
    (require 'doom-modeline)
    (doom-modeline-def-modeline
      'agcustom
      '(bar persp-name workspace-name buffer-info)
      '(;; battery
        ;; grip
        ;; irc mu4e debug
        ;; repl
        lsp
        major-mode
        misc-info
        ;; process
        ;; checker
        ;; buffer-position
        matches selection-info))

    (defun setup-custom-doom-modeline ()
      (doom-modeline-set-modeline 'agcustom))

    (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline 90)
    (add-hook 'after-change-major-mode-hook 'setup-custom-doom-modeline 90)

    (setq doom-modeline-buffer-encoding nil
          doom-modeline-buffer-file-name-style 'relative-from-project
          doom-modeline-buffer-modification-icon t
          doom-modeline-buffer-state-icon t
          doom-modeline-icon (display-graphic-p)
          doom-modeline-major-mode-color-icon nil
          doom-modeline-major-mode-icon nil
          doom-modeline-modal-icon nil
          doom-modeline-mu4e nil
          doom-modeline-persp-icon t
          doom-modeline-display-default-persp-name t
          inhibit-compacting-font-caches t
          doom-modeline-height 15
          doom-modeline-bar-width 2)
    :config
    (doom-modeline-mode +1)
    (defun doom-modeline--font-height () 5)))

(defun ag-general/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)
    ;; remove rectangles from around the colors
    (setq css-fontify-colors nil)))

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
        (select-frame-by-name "edit")
        (with-current-buffer (get-buffer buffer-name)
          (set-frame-parameter nil 'fullscreen nil)
          ;; need to set a filename, otherwise lsp in that buffer won't work
          (set-visited-file-name (format "/tmp/%s_%s_%s" buffer-name pid title))
          (set-buffer-modified-p nil)
          (spacemacs/evil-search-clear-highlight)
          (spacemacs/toggle-visual-line-navigation-on)
          (markdown-mode)
          (evil-insert 1)))

      (add-hook 'spacehammer/edit-with-emacs-hook #'on-spacehammer-edit-with-emacs)

      (defun spacehammer-before-finish-edit-with-emacs (bufname pid)
        (with-current-buffer bufname
         (set-buffer-modified-p nil)))

      (add-hook 'spacehammer/before-finish-edit-with-emacs-hook #'spacehammer-before-finish-edit-with-emacs)

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

(defun ag-general/init-company-tabnine ()
  (use-package company-tabnine
    :ensure t
    :config
    (spacemacs|add-company-backends
      :backends company-tabnine
      :modes sql-mode sql-interactive-mode)))

(defun ag-general/init-company-posframe ()
  (use-package company-posframe
    :hook (company-mode . company-posframe-mode)
    :init
    (setq company-posframe-quickhelp-delay 1
          company-quickhelp-delay nil)
    :bind (:map company-active-map
                ("C-h" . (lambda () (interactive) (company-posframe-quickhelp-show)))
                ("C-c C-d". company-show-doc-buffer)
                ("C-n" . company-select-next)
                ("C-p" . company-select-previous)
                ("C-c C-l". company-show-location)
                :map company-posframe-active-map
                ("C-c h" . company-posframe-quickhelp-toggle)
                ("C-n" . company-select-next)
                ("C-p" . company-select-previous)
                :map company-search-map
                ("C-n" . company-select-next)
                ("C-p" . company-select-previous))
    :config
    (company-posframe-mode 1)
    ;; doom-modeline keeps re-rendendering through company compleiton changes
    (add-hook 'company-completion-started-hook (lambda (manual-p) (doom-modeline-mode -1)))
    (add-hook 'company-completion-finished-hook #'doom-modeline-mode)
    (add-hook 'company-completion-cancelled-hook #'doom-modeline-mode)))

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
          er/mark-between
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
          er/mark-defun)))

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
       marginalia-annotators-light))

    ;; it otherwise breaks the alignment on ultra-wide
    (marginalia-margin-threshold 350)))

(with-eval-after-load 'terminal-here
  (setq terminal-here-terminal-command
        (lambda (dir)
          (cond
           ((eq system-type 'darwin)
            (list "open" "-a" "iTerm" dir))))))

(with-eval-after-load 'ivy
  (defun ivy--recenter-after-press ()
    (with-ivy-window (recenter)))

  (advice-add 'ivy-occur-press :after 'ivy--recenter-after-press)
  (add-hook 'ivy-occur-grep-mode-hook #'evil-evilified-state))

(add-hook 'Info-selection-hook #'evil-evilified-state)

(defun ag-general/init-ivy-prescient ()
  (use-package ivy-prescient
    :after ivy
    :config
    (ivy-prescient-mode +1)))

(defun ag-general/init-company-prescient ()
  (use-package company-prescient
    :after company
    :hook
    (company-mode . company-prescient-mode)))

;;; packages.el ends here
