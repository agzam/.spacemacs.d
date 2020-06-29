;;; packages.el --- ag-org layer packages -*- lexical-binding: t; -*-
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
                            clocker
                            org-noter
                            (latex-fragments :location local)
                            (org-present :excluded t)
                            (org-journal :excluded t)
                            (org-brain :excluded t)
                            ;; (slack2org :location local)
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
     org-read-date-popup-calendar t
     org-capture-templates
          '(("T" "todo" entry (file+olp+datetree "~/Dropbox/org/tasks.org")
             "* TODO %i %?\nSCHEDULED: %T\n"
             :time-prompt t
             )

            ("t" "Today" entry (file+olp+datetree "~/Dropbox/org/tasks.org")
             "* TODO %?\nSCHEDULED: %(org-read-date t nil nil nil (current-time))\n")

            ("i" "Immediate" entry (file+olp+datetree "~/Dropbox/org/tasks.org")
             "* ONGOING %i %?" :clock-in t :clock-resume t :clock-keep t)

            ("s" "Someday" entry (file+headline "~/Dropbox/org/tasks.org" "Tasks")
             "* TODO %?\nSCHEDULED: %(org-read-date t nil nil nil (current-time) \"+1m\")\n")

            ("c" "Code Snippet" entry (file+olp+datetree "~/Dropbox/org/tasks.org")
             ;; Prompt for tag and language
             "* %u  %?\n\t%F\n\t#+BEGIN_SRC %^{language}\n\t\t%i\n\t#+END_SRC")

            ("y" "Yakety" entry (file "~/Dropbox/org/yakety.org")
             "* TODO  %?\n SCHEDULED: %^u\n :LOGBOOK:\n  - State \"TODO\" from %U\n :END:")

            ("j" "Journal" entry (file+olp+datetree "~/Dropbox/org/journal.org")
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
     org-bullets-bullet-list '("⏣" "◉" "●" "•" "‣" "⋄" "∙")
     org-goto-interface 'outline-path-completion  ; org-goto C-c C-j like in org-refile
     org-startup-folded t
     org-blank-before-new-entry nil
     org-ellipsis " ↴"
     org-M-RET-may-split-line '((headline))
     org-ctrl-k-protect-subtree t
     org-catch-invisible-edits 'smart
     org-use-property-inheritance nil
     org-hide-emphasis-markers t
     org-special-ctrl-a/e t
     org-tags-column -80
     org-startup-indented nil
     org-fontify-whole-heading-line t
     org-fontify-done-headline t
     )

    (add-hook 'org-reveal-start-hook 'end-of-visual-line)

     ;;;; ---- lists ----
    (setq
     org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+"))
     org-list-allow-alphabetical t)

     ;;;; ---- agenda ----
    (setq
     org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 60)
     org-agenda-diary-file "~/Dropbox/org/journal.org"
     org-agenda-files "~/Dropbox/org/.agenda-files"
     org-agenda-follow-indirect t
     org-agenda-persistent-marks t
     org-agenda-skip-deadline-if-done t
     org-agenda-skip-scheduled-if-done t
     org-agenda-span 1
     org-agenda-start-with-clockreport-mode t
     org-agenda-start-with-follow-mode nil
     org-agenda-start-with-log-mode t)

     ;;;; ---- todo ----
    (setq
     org-confirm-babel-evaluate nil
     org-todo-keywords '((sequence "TODO" "ONGOING" "DONE"))
     org-todo-keyword-faces '(("ONGOING" . "orange"))
     org-enforce-todo-dependencies t
     org-enforce-todo-checkbox-dependencies t
     org-hierarchical-todo-statistics t
     )

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

     ;;;; ---- tags ----
    (setq org-fast-tag-selection-single-key t)

     ;;;; ---- src blocks ----
    (setq
     org-src-window-setup 'other-window
     org-src-ask-before-returning-to-edit-buffer nil
     ;; org-src-preserve-indentation t
     )

     ;;;; ----- clocking ----
    (setq
     org-clock-persist t
     org-clock-in-switch-to-state "ONGOING"
     org-clock-persist-query-resume nil
     org-clock-report-include-clocking-task t
     org-clock-out-remove-zero-time-clocks t
     org-clock-idle-time 10)

    ;;;; To save the clock history across Emacs sessions, use
    (org-clock-persistence-insinuate)

     ;;;; ----- export ----
    (setq
     org-export-with-toc nil
     org-export-with-section-numbers nil)

    ;;;; ----- abo-abo/org-download -------
    (setq-default org-download-image-dir (concat org-directory "/images")
                  org-download-heading-lvl nil
                  org-download-image-org-width 500)

    ;;;; ----- misc -------
    (setq
     org-refile-use-outline-path 'file
     org-outline-path-complete-in-steps nil
     org-refile-targets '((nil :maxlevel . 3)
                          (org-agenda-files :maxlevel . 3))
     org-refile-allow-creating-parent-nodes 'confirm

     org-log-states-order-reversed nil
     org-reverse-note-order nil
     org-log-into-drawer t
     org-log-note 'time
     org-enable-github-support t
     org-enable-bootstrap-support t
     org-format-latex-options (plist-put org-format-latex-options :scale 2)
     org-format-latex-options (plist-put org-format-latex-options :background nil))

    (add-to-list 'auto-mode-alist '("\\Dropbox/org/.*\.txt\\'" . org-mode))

    ;;;; evilified state rebinds C-u in agenda
    (defun override-evilified-keys ()
      (evil-define-key 'evilified org-agenda-mode-map "\C-u" 'universal-argument))
    (add-hook 'org-agenda-mode-hook 'override-evilified-keys)

    (add-hook 'org-mode-hook #'flyspell-mode)
    (add-hook 'org-mode-hook #'ag/org-mode-hook)
    (add-hook 'org-mode-hook #'spacemacs/toggle-visual-line-navigation-on)
    (add-hook 'org-mode-hook #'org-indent-mode)
    (remove-hook 'org-mode-hook #'spacemacs/delay-emoji-cheat-sheet-hook)

    ;; (add-hook 'org-timer-done-hook (lambda () (spacemacs/alert "-- timer done! --")))

    ;; (require 'ob-http)
    (require 'ob-clojure)
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

    (add-hook 'org-mode-hook #'abbrev-mode)

    (add-hook 'persp-before-switch-functions 'autosave-tasks-org)

    (add-to-list 'org-modules 'org-tempo t)

    ;; Smarter org-return in lists. RET in plain lists would insert items, or
    ;; checkboxes - depending on the current context. C-j splits the line.
    (defun org-return--around (old-fn &rest args)
      (let ((context (org-element-lineage (org-element-at-point) '(item))))
        (if (and context (not args))
            (org-insert-item (org-element-property :checkbox context))
          (apply old-fn args))))

    (advice-add 'org-return :around 'org-return--around)

    (setq org-link-make-description-function 'org-link-make-description-function*)))

(defun ag-org/post-init-org-pomodoro ()
  (with-eval-after-load 'org-pomodoro
    (setq org-pomodoro-keep-killed-pomodoro-time t)
    (add-hook 'org-pomodoro-finished-hook #'pomodoro/on-finished-hook)
    (add-hook 'org-pomodoro-break-finished-hook #'pomodoro/on-break-over-hook)
    (add-hook 'org-pomodoro-killed-hook #'pomodoro/on-killed-hook)
    (add-hook 'org-pomodoro-started-hook #'pomodoro/on-started-hook)))

(defun ag-org/init-org-noter ()
  (use-package org-noter
    :config
    (setq org-noter-always-create-frame nil
          org-noter-insert-note-no-questions t
          org-noter-separate-notes-from-heading t
          org-noter-auto-save-last-location t
          org-noter-kill-frame-at-session-end t
          org-noter-notes-search-path '("~/Dropbox/org"
                                        "~/SyncMobile/Books"
                                        "~/SyncMobile/Books/Papers")
          org-noter-default-notes-file-names '("notes.org"))

    (defun org-noter-init-pdf-view ()
      (pdf-view-fit-page-to-window)
      (pdf-view-auto-slice-minor-mode)
      (run-at-time "0.5 sec" nil #'org-noter))

    (add-hook 'pdf-view-mode-hook 'org-noter-init-pdf-view)

    (defun org-noter-kill-the-note-buffer (&rest _ignore)
      (setq notes-fname (org-noter--session-notes-file-path org-noter--session))
      (setq pdf-fname (buffer-file-name (org-noter--session-doc-buffer org-noter--session)))
      (run-at-time "0.5 sec" nil (lambda ()
                                   (kill-buffer (get-file-buffer pdf-fname))
                                   (switch-to-buffer (get-file-buffer notes-fname))
                                   (kill-buffer (get-file-buffer notes-fname))
                                   (makunbound 'notes-fname)
                                   (makunbound 'pdf-fname))))

    (advice-add #'org-noter-kill-session :before 'org-noter-kill-the-note-buffer)))
(defadvice spacemacs/mode-line-prepare-left (around compile)
  (setq ad-return-value (clocker-add-clock-in-to-mode-line ad-do-it)))

(defun ag-org/init-clocker ()
  (use-package clocker
    :config
    (progn
      (setq clocker-keep-org-file-always-visible nil)
      (add-to-list 'clocker-skip-after-save-hook-on-file-name '("~/.emacs.d/.cache/recentf"))
      (ad-activate 'spacemacs/mode-line-prepare-left)
      ;; (add-hook 'prog-mode-hook 'clocker-mode)
      )))

(defun ag-org/init-ox-reveal ()
  (use-package ox-reveal
    :defer t
    :config
    (progn
      (setq
       org-enable-reveal-js-support t
       org-reveal-title-slide nil))))

(defun ag-org/init-latex-fragments ()
  (use-package latex-fragments
    :demand t))

(defun ag-org/init-slack2org ()
  (use-package slack2org
    :demand t))

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
