(defconst ag-org-packages '(org
                            org-pomodoro
                            ;; ox-reveal
                            (org-present :excluded t)
                            (org-journal :excluded t)
                            (org-brain :excluded t)))

(defun ag-org/post-init-org ()
  ;; (with-eval-after-load 'evil-org-mode
    ;; Temporary fix for `o` on collapsed sections
    ;; until https://github.com/syl20bnr/spacemacs/pull/8200 is fixed
  ;;  (defun evil-org-eol-call (fun)
  ;;    "Go to end of line and call provided function.
  ;;    FUN function callback"
  ;;    (end-of-visible-line)
  ;;    (funcall fun)
  ;;    (evil-append nil)))

  (with-eval-after-load 'org
    (setq org-capture-templates
     '(("t" "Todo" entry (file "~/Dropbox/org/tasks.org")
        "* TODO  %?\n  :LOGBOOK:\n  - State \"TODO\"       from              %U\n  :END:")
       ("c" "Code Snippet" entry (file "~/Dropbox/org/tasks.org")
        ;; Prompt for tag and language
        "* %u  %?\n\t%f\n\t#+BEGIN_SRC %^{language}\n\t\t%i\n\t#+END_SRC")
       ("y" "Yakety" entry (file "~/Dropbox/org/yakety.org")
        "* TODO  %?\n  :LOGBOOK:\n  - State \"TODO\"       from              %U\n  :END:")
       ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
        "* %u %?"
        :time-prompt t)
       ("z" "Currently clocked-in" item (clock)
        "Note taken on %U \\\ \n%?")))

    (setq
     org-directory "~/Dropbox/org"
     org-startup-folded t
     org-M-RET-may-split-line '((headline))
     ort/prefix-arg-directory "~/Dropbox/org"
     org-blank-before-new-entry nil
     org-default-notes-file "~/Dropbox/org/notes.org"
     org-show-notification-handler 'message
     org-id-locations-file (concat org-directory "/.org-id-locations")
     org-ctrl-k-protect-subtree t
     org-catch-invisible-edits 'smart
     ;; https://github.com/syl20bnr/spacemacs/issues/8455
     org-src-fontify-natively nil
     org-src-window-setup 'current-window
     ;; org-src-preserve-indentation t
     org-src-ask-before-returning-to-edit-buffer nil

     org-refile-targets '((nil :maxlevel . 3)
                          (org-agenda-files :maxlevel . 3))
     org-refile-allow-creating-parent-nodes 'confirm

     ;; agenda
     org-agenda-files "~/Dropbox/org/.agenda-files"
     org-agenda-span 3
     org-agenda-skip-scheduled-if-done t
     org-agenda-skip-deadline-if-done t

     ;;;; todo
     org-confirm-babel-evaluate nil
     org-todo-keywords (quote ((sequence "TODO" "ONGOING" "DONE")))
     org-todo-keyword-faces '(("ONGOING" . "orange"))
     org-enforce-todo-dependencies t
     org-enforce-todo-checkbox-dependencies t

     org-log-states-order-reversed nil
     org-reverse-note-order t
     org-log-into-drawer t
     org-clock-persist t
     org-clock-in-switch-to-state "ONGOING"
     org-clock-persist-query-resume nil
     org-clock-report-include-clocking-task t)

    (add-to-list 'auto-mode-alist '("\\Dropbox/org/.*\.txt\\'" . org-mode))

    ;; To save the clock history across Emacs sessions, use
    (org-clock-persistence-insinuate)

    ;; (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook #'ag/org-mode-hook)
    (add-hook 'org-mode-hook #'spacemacs/toggle-visual-line-navigation-on)
    (add-hook 'org-timer-done-hook (lambda () (hs-alert "-- timer done! --")))

    ;; (require 'ob-http)
    ;; (require 'ob-clojure)
    ;; (require 'ob-ruby)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (js . t)
       (clojure . t)
       (ruby . t)))

    (add-hook 'org-babel-post-tangle-hook #'ag/set-tangled-file-permissions)
    (add-hook 'org-mode-hook #'abbrev-mode)

    ;; lower-casing tab-expanded options e.g.: <s
    (mapc (lambda (arg) (setcdr arg (list (downcase (cadr arg)))))
          org-structure-template-alist)))

(defun ag-org/post-init-org-pomodoro ()
  (with-eval-after-load 'org-pomodoro
    (setq org-pomodoro-keep-killed-pomodoro-time t)
    (add-hook 'org-pomodoro-finished-hook #'pomodoro/on-finished-hook)
    (add-hook 'org-pomodoro-break-finished-hook #'pomodoro/on-break-over-hook)
    (add-hook 'org-pomodoro-killed-hook #'pomodoro/on-killed-hook)
    (add-hook 'org-pomodoro-started-hook #'pomodoro/on-started-hook)))

(defun ag-org/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :config
    (progn
      (setq org-reveal-title-slide nil))))
