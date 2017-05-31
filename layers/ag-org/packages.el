(defconst ag-org-packages '(org
                            org-pomodoro
                            ;; ox-reveal
			    ))

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
    (setq
     org-directory "~/Dropbox/org"
     org-startup-folded t
     org-log-into-drawer t
     org-M-RET-may-split-line '((headline))
     org-capture-templates
     '(("t" "Todo" entry (file (concat org-directory "/tasks.org"))
        "* TODO %t  %?\n\t%i\n")
       ("c" "Code Snippet" entry (file (concat org-directory "/yakety.org"))
        ;; Prompt for tag and language
        "* %u  %?\n\t%f\n\t#+BEGIN_SRC %^{language}\n\t\t%i\n\t#+END_SRC"))

     ort/prefix-arg-directory "~/Dropbox/org"
     org-agenda-files '("~/Dropbox/org/tasks.org"
                        "~/Dropbox/org/yakety.org"
                        "~/Dropbox/org/pocket.org.txt")

     org-refile-targets '((nil :maxlevel . 3)
                          (org-agenda-files :maxlevel . 3))
     org-refile-allow-creating-parent-nodes 'confirm

     org-default-notes-file "tasks.org"
     ;; I don't want to be prompted on every code block evaluation
     org-confirm-babel-evaluate nil
     org-todo-keywords (quote ((sequence "TODO" "ONGOING" "DONE")))
     org-todo-keyword-faces '(("ONGOING" . "orange"))

     ;; Do not prompt to resume an active clock
     org-clock-persist-query-resume nil
     ;; Include current clocking task in clock reports
     org-clock-report-include-clocking-task t
     org-enforce-todo-dependencies t
     org-enforce-todo-checkbox-dependencies t
     org-id-locations-file (concat org-directory "/.org-id-locations")

     org-ctrl-k-protect-subtree t
     org-catch-invisible-edits 'smart

     ;; https://github.com/syl20bnr/spacemacs/issues/8455
     org-src-fontify-natively nil
     )
    (add-to-list 'auto-mode-alist '("\\Dropbox/org/.*\.txt\\'" . org-mode))


    ;; To save the clock history across Emacs sessions, use
    (org-clock-persistence-insinuate)

    ;; (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook #'ag/org-mode-hook)
    (add-hook 'org-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
    (add-hook 'org-timer-done-hook (lambda () (hs-alert "-- timer done! --")))

    (require 'ob-http)
    (require 'ob-clojure)
    (require 'ob-ruby)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (js . t)
       (clojure . t)
       (ruby . t)))))

(defun ag-org/post-init-org-pomodoro ()
  (with-eval-after-load 'org-pomodoro
    (add-hook 'org-pomodoro-finished-hook 'pomodoro/on-finished-hook)
    (add-hook 'org-pomodoro-break-finished-hook 'pomodoro/on-break-over-hook)
    (add-hook 'org-pomodoro-killed-hook 'pomodoro/on-killed-hook)
    (add-hook 'org-pomodoro-started-hook 'pomodoro/on-started-hook)))

(defun ag-org/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :config
    (progn
      (setq org-reveal-title-slide nil))))
