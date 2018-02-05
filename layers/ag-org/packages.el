;;; packages.el --- ag-org layer packages
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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
        "* TODO  %? %^u")
       ("i" "Immediate" entry (file "~/Dropbox/org/tasks.org")
        "* ONGOING %? %U" :clock-in t :clock-resume t :clock-keep t)
       ("c" "Code Snippet" entry (file "~/Dropbox/org/tasks.org")
        ;;;; Prompt for tag and language
        "* %u  %?\n\t%F\n\t#+BEGIN_SRC %^{language}\n\t\t%i\n\t#+END_SRC")
       ("y" "Yakety" entry (file "~/Dropbox/org/yakety.org")
        "* TODO  %?\n  :LOGBOOK:\n  - State \"TODO\"       from              %U\n  :END:")
       ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
        "* %u %?"
        :time-prompt t)
       ("z" "Currently clocked-in" item (clock)
        "Note taken on %U \\\ \n%?")))

    (setq
     org-directory "~/Dropbox/org"
     ort/prefix-arg-directory "~/Dropbox/org"
     org-default-notes-file "~/Dropbox/org/notes.org"
     org-show-notification-handler 'message
     org-id-locations-file (concat org-directory "/.org-id-locations"))

     ;;;; ---- headings ----
    (setq
     org-goto-interface 'outline-path-completion        ;; org-goto C-c C-j like in org-refile
     org-startup-folded t
     org-blank-before-new-entry nil
     org-ellipsis " ↴"
     org-M-RET-may-split-line '((headline))
     org-ctrl-k-protect-subtree t
     org-catch-invisible-edits 'smart)

     ;;;; ---- lists ----
    (setq
     org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
     org-list-allow-alphabetical t)

     ;;;; ---- agenda ----
    (setq
     org-agenda-files "~/Dropbox/org/.agenda-files"
     org-agenda-span 3
     org-agenda-skip-scheduled-if-done t
     org-agenda-skip-deadline-if-done t
     org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 60)
     org-agenda-diary-file "~/Dropbox/org/journal.org")

    ;; evilified state rebinds C-u in agenda
    (defun override-evilified-keys ()
      (evil-define-key 'evilified org-agenda-mode-map "\C-u" 'universal-argument))
    (add-hook 'org-agenda-mode-hook 'override-evilified-keys)

     ;;;; ---- todo ----
    (setq
     org-confirm-babel-evaluate nil
     org-todo-keywords (quote ((sequence "TODO" "ONGOING" "DONE")))
     org-todo-keyword-faces '(("ONGOING" . "orange"))
     org-enforce-todo-dependencies t
     org-enforce-todo-checkbox-dependencies t)

     ;;;; ---- tags ----
    (setq org-fast-tag-selection-single-key t)

     ;;;; ---- src blocks ----
    (setq
     org-src-fontify-natively nil                     ;; https://github.com/syl20bnr/spacemacs/issues/8455
     org-src-window-setup 'current-window
     org-src-ask-before-returning-to-edit-buffer nil
     ;; org-src-preserve-indentation t
     )

    (setq
     org-refile-use-outline-path 'file
     org-outline-path-complete-in-steps nil
     org-refile-targets '((nil :maxlevel . 3)
                          (org-agenda-files :maxlevel . 3))
     org-refile-allow-creating-parent-nodes 'confirm

     org-log-states-order-reversed nil
     org-reverse-note-order t
     org-log-into-drawer t)

     ;;;; ----- clocking ----
    (setq
     org-clock-persist t
     org-clock-in-switch-to-state "ONGOING"
     org-clock-persist-query-resume nil
     org-clock-report-include-clocking-task t
     org-clock-out-remove-zero-time-clocks t)

    (add-to-list 'auto-mode-alist '("\\Dropbox/org/.*\.txt\\'" . org-mode))

    ;;;; To save the clock history across Emacs sessions, use
    (org-clock-persistence-insinuate)

    ;; (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook #'ag/org-mode-hook)
    (add-hook 'org-mode-hook #'spacemacs/toggle-visual-line-navigation-on)
    (add-hook 'org-timer-done-hook (lambda () (hs-alert "-- timer done! --")))

    ;; (require 'ob-http)
    ;; (require 'ob-clojure)
    (require 'ob-ditaa)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (js . t)
       (clojure . t)
       (sql . t)
       (shell . t)
       (ditaa . t)
       (ruby . t)))

    ;; I don't know any better way of determining path to ditaa, other than running `find / -name "ditaa*.jar" 2>/dev/null`
    (setq org-ditaa-jar-path
     (pcase system-type
       ('gnu/linux "/usr/share/java/ditaa/ditaa-0_10.jar")
       ('darwin  "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar")))

    (add-hook 'org-babel-post-tangle-hook #'ag/set-tangled-file-permissions)
    (add-hook 'org-mode-hook #'abbrev-mode)

    ;;;; lower-casing tab-expanded options e.g.: <s
    (mapc (lambda (arg) (setcdr arg (list (downcase (cadr arg)))))
          org-structure-template-alist))

  )

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

(with-eval-after-load 'artist
  ;;; artist mode doesn't work properly in evil-mode
  ;;; see: https://github.com/syl20bnr/spacemacs/issues/4926
  (defun artist-mode-toggle-emacs-state ()
    (if artist-mode
        (evil-emacs-state)
      (evil-exit-emacs-state)))
  (unless (eq dotspacemacs-editing-style 'emacs)
    (add-hook 'artist-mode-hook #'artist-mode-toggle-emacs-state))

  ;;; right-click bound to middle click in artist mode
  (define-key artist-mode-map [(down-mouse-3)] 'artist-mouse-choose-operation))

;;; packages.el ends here
