(defconst ag-org-packages '(org 
                            org-pomodoro
                            ;; ox-reveal
			    ))

(defun ag-org/post-init-org ()
  (with-eval-after-load 'org 
    (setq
     org-startup-folded t
     org-log-into-drawer t
     org-M-RET-may-split-line '((headline))
     org-capture-templates
     '(("t" "Todo" entry (file (concat org-directory "/tasks.org"))
        "* TODO %t  %?\n\t%i\n")
       ("c" "Code Snippet" entry
        (file (concat org-directory "/tasks.org"))
        ;; Prompt for tag and language
        "* %t  %?\n\t%f\n\t#+BEGIN_SRC %^{language}\n\t\t%i\n\t#+END_SRC"))
     
     ort/prefix-arg-directory "~/Google Drive/org"
     org-agenda-files '("~/Google Drive/org/tasks.org")
     org-default-notes-file "tasks.org"
     ;; I don't want to be prompted on every code block evaluation
     org-confirm-babel-evaluate nil
     org-todo-keywords (quote ((sequence "TODO" "ONGOING" "DONE")))
     org-todo-keyword-faces '(("ONGOING" . "orange")))

    ;; To save the clock history across Emacs sessions, use
    (org-clock-persistence-insinuate)

    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

    (require 'ob-http)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
       (js . t)
       (clojure . t)))))

(defun ag-org/post-init-org-pomodoro ()
  (with-eval-after-load 'org-pomodoro
    (add-hook 'org-pomodoro-finished-hook 'pomodoro/on-finished-hook)
    (add-hook 'org-pomodoro-break-finished-hook 'pomodoro/on-break-over-hook)
    (add-hook 'org-pomodoro-killed-hook 'pomodoro/on-killed-hook)
    (add-hook 'org-pomodoro-started-hook 'pomodoro/on-started-hook)))

(defun ag-org/init-ox-reveal ()
  (use-package ox-reveal
    :config
    (progn
      (setq org-reveal-title-slide nil))))
