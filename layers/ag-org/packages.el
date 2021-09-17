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

(defconst ag-org-packages
  '(org
    org-pomodoro
    ;; ox-reveal
    clocker
    (latex-fragments :location local)
    (org-present :excluded t)
    (org-journal :excluded t)
    (org-brain :excluded t)
    ;; (slack2org :location local)
    anki-editor
    (org-roam :location (recipe :fetcher github
                                :repo "org-roam/org-roam"
                                :files ("*.el" "extensions")))
    (org-ref :location (recipe :fetcher github
                               :repo "jkitchin/org-ref"))
    (org-edit-indirect :location local)
    (org-roam-ui :location (recipe :fetcher github
                                   :repo "org-roam/org-roam-ui"
                                   :files ("*.el" "out")))
    expand-region))

(add-to-list 'load-path (concat (configuration-layer/get-elpa-package-install-directory 'org-roam) "extensions"))

(setq org-default-folder "~/Dropbox/org/"
      org-roam-v2-ack t
      org-appear-delay 0.5)

(setq org-default-main-file (concat org-default-folder "tasks.org"))

(defun ag-org/post-init-org ()
  (add-hook 'before-save-hook 'org-roam--set-last-modified)
  (with-eval-after-load 'org-capture
    (require 'org-roam-dailies)
    (require 'org-roam-protocol)
    (remove-hook 'org-capture-mode-hook 'spacemacs//org-capture-start)
    (dolist (template
             `(("T" "todo" entry (file+olp+datetree org-default-main-file)
                "* TODO %i %?\nSCHEDULED: %T\n"
                :time-prompt t)

               ("t" "Today" entry (file+olp+datetree org-default-main-file)
                "* TODO %?\nSCHEDULED: %(org-read-date t nil nil nil (current-time))\n")

               ("i" "Immediate" entry (file+olp+datetree org-default-main-file)
                "* ONGOING %i %?" :clock-in t :clock-resume t :clock-keep t)

               ("s" "Someday" entry (file+headline org-default-main-file "Tasks")
                "* TODO %?\nSCHEDULED: %(org-read-date t nil nil nil (current-time) \"+1m\")\n")

               ("c" "Code Snippet" entry (file+olp+datetree ,org-default-main-file)
                ;; Prompt for tag and language
                "* %u  %?\n\t%F\n\t#+BEGIN_SRC %^{language}\n\t\t%i\n\t#+END_SRC")

               ("y" "Yakety" entry (file ,(concat org-default-folder "yakety.org"))
                "* TODO  %?\n SCHEDULED: %^u\n :LOGBOOK:\n  - State \"TODO\" from %U\n :END:")

               ("j" "Journal" entry (file+olp+datetree ,(concat org-default-folder "journal.org"))
                "* %u %?"
                :time-prompt t
                :jump-to-captured t)

               ("g" "Gym Workout" entry (file+olp+datetree ,(concat org-default-folder "workouts.org"))
                "* %u %?No exercise day"
                :time-prompt t
                :tree-type month
                :jump-to-captured t)

               ("z" "Currently clocked-in" item (clock)
                "Note taken on %U \\\ \n%?")))
      (add-to-list 'org-capture-templates template)))

  (with-eval-after-load 'org
    (require 'org-tempo)
    (add-to-list 'org-modules 'org-tempo t)

    (setq
     org-read-date-popup-calendar t

     org-directory "~/Dropbox/org"
     ort/prefix-arg-directory "~/Dropbox/org"
     org-default-notes-file "~/Dropbox/org/notes.org"
     org-show-notification-handler 'message
     org-id-locations-file (concat org-directory "/.org-id-locations"))

     ;;;; ---- headings ----
    (setq
     org-bullets-bullet-list '("⏣" "◉" "●" "•" "‣" "⋄" "∙")
     org-goto-interface 'outline-path-completion  ; org-goto C-c C-j like in org-refile
     org-startup-folded 'nofold
     org-blank-before-new-entry nil
     org-ellipsis " ↴"
     org-M-RET-may-split-line '((headline))
     org-ctrl-k-protect-subtree t
     org-catch-invisible-edits 'smart
     org-use-property-inheritance nil
     org-hide-emphasis-markers t
     org-pretty-entities t
     org-pretty-entities-include-sub-superscripts nil
     org-special-ctrl-a/e t
     org-tags-column -80
     org-startup-indented nil
     org-fontify-whole-heading-line t
     org-fontify-done-headline t)

    (add-hook 'org-reveal-start-hook 'end-of-visual-line)
    (add-hook 'occur-mode-find-occurrence-hook 'outline-show-subtree)

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
     org-todo-keywords '((sequence "TODO(t!)" "ONGOING(o!)" "|" "DONE(d)" "CANCELED(c@/!)"))
     org-todo-keyword-faces '(("ONGOING" . "orange"))
     org-enforce-todo-dependencies t
     org-enforce-todo-checkbox-dependencies t
     org-hierarchical-todo-statistics t)

     ;;;; ---- tables ----
    (setq org-table-automatic-realign nil)

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
     org-src-tab-acts-natively nil
     org-edit-src-content-indentation 0
     org-fontify-quote-and-verse-blocks t
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

     org-log-states-order-reversed t
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
    (add-hook 'org-mode-hook #'smartparens-mode)
    (add-hook 'org-mode-hook #'spacemacs/toggle-visual-line-navigation-on)
    (add-hook 'org-mode-hook #'org-indent-mode)
    (add-hook 'org-agenda-after-show-hook #'recenter)
    (remove-hook 'org-mode-hook #'spacemacs/delay-emoji-cheat-sheet-hook)
    (remove-hook 'org-mode-hook #'valign-mode)

    ;; (add-hook 'org-timer-done-hook (lambda () (spacemacs/alert "-- timer done! --")))

    ;; (require 'ob-http)
    (require 'ob-clojure)
    (require 'ob-ditaa)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (js . t)
       (python . t)
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

    (advice-add 'org-return :around 'org-return--around)

    (setq org-link-make-description-function #'org-link-make-description-function*))

  (with-eval-after-load 'org-attach
    ;; borrowed from [[https://www.youtube.com/watch?v=4iO7SbGhXoQ&t][Emacs org-attach basics - YouTube]]
    (add-hook 'org-attach-after-change-hook #'org-attach-save-file-list-to-property))

  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key "E099A30039C121430579BA65CFE12444AF47BD1"))

(defun ag-org/post-init-org-pomodoro ()
  (with-eval-after-load 'org-pomodoro
    (setq org-pomodoro-keep-killed-pomodoro-time t)
    (add-hook 'org-pomodoro-finished-hook #'pomodoro/on-finished-hook)
    (add-hook 'org-pomodoro-break-finished-hook #'pomodoro/on-break-over-hook)
    (add-hook 'org-pomodoro-killed-hook #'pomodoro/on-killed-hook)
    (add-hook 'org-pomodoro-started-hook #'pomodoro/on-started-hook)))

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
    ;; :demand t
    ))

(defun ag-org/init-anki-editor ()
  ;; Borrowed ideas from https://yiufung.net/post/anki-org
  (use-package anki-editor
    :after org
    :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
    :init
    (setq org-my-anki-file (concat org-default-folder "anki_cards.org"))
    (with-eval-after-load 'org-capture
      (dolist (template
               '(("a" "Anki cards")
                 ("ab" "Anki basic"
                  entry
                  (file+headline org-my-anki-file "Dispatch")
                  "* %^{prompt|card %<%Y-%m-%d %H:%M>} %^g%^{ANKI_DECK}p\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:END:\n** Front\n%?\n** Back\n%x\n"
                  :jump-to-captured t)
                 ("ar" "Anki basic & reversed"
                  entry
                  (file+headline org-my-anki-file "Dispatch")
                  "* %^{prompt|card %<%Y-%m-%d %H:%M>} %^g%^{ANKI_DECK}p\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:END:\n** Front\n%?\n** Back\n%x\n"
                  :jump-to-captured t)
                 ("ac" "Anki cloze"
                  entry
                  (file+headline org-my-anki-file "Dispatch")
                  "* %^{prompt|card %<%Y-%m-%d %H:%M>} %^g%^{ANKI_DECK}p\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:END:\n** Text\n%?\n** Extra\n%x\n"
                  :jump-to-captured t)))
        (add-to-list 'org-capture-templates template)))

    (defun anki-editor-set-org-capture-keys ()
      (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
        dotspacemacs-major-mode-leader-key 'org-capture-finalize
        "a" nil
        "k" nil
        "kc" 'anki-editor-cloze-region-dont-incr
        "kC" 'anki-editor-cloze-region-auto-incr
        "kr" 'anki-editor-reset-cloze-number
        "kp" 'anki-editor-push-tree))

    (add-hook 'org-capture-mode-hook 'anki-editor-set-org-capture-keys t)
    :config
    (setq anki-editor-create-decs t ; Allow anki-editor to create a new deck if it doesn't exist
          anki-editor-org-tags-as-anki-tags t)

    (defun anki-editor-cloze-region-auto-incr (&optional arg)
      "Cloze region without hint and increase card number."
      (interactive)
      (anki-editor-cloze-region my-anki-editor-cloze-number "")
      (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
      (forward-sexp))

    (defun anki-editor-cloze-region-dont-incr (&optional arg)
      "Cloze region without hint using the previous card number."
      (interactive)
      (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
      (forward-sexp))

    (defun anki-editor-reset-cloze-number (&optional arg)
      "Reset cloze number to ARG or 1"
      (interactive)
      (setq my-anki-editor-cloze-number (or arg 1)))

    (defun anki-editor-push-tree ()
      "Push all notes under a tree."
      (interactive)
      (anki-editor-push-notes '(4))
      (anki-editor-reset-cloze-number))

    ;; Initialize
    (anki-editor-reset-cloze-number)

    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "kc" 'anki-editor-cloze-region-dont-incr
      "kC" 'anki-editor-cloze-region-auto-incr
      "kr" 'anki-editor-reset-cloze-number
      "kp" 'anki-editor-push-tree)))

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

(with-eval-after-load 'org-superstar
  (setq org-superstar-item-bullet-alist
    '((?* . ?⋆)
      (?+ . ?◦)
      (?- . ?•))))

;;;;;;;;;;;;;;
;; Org-roam ;;
;;;;;;;;;;;;;;

(setq
 org-roam-directory "~/Dropbox/org"
 org-roam-db-location "~/Dropbox/org/org-roam.db"
 org-roam-graph-viewer "/usr/bin/open"
 org-roam-dailies-directory "daily/")

(defun ag-org/post-init-org-roam ()
  ;; (add-hook 'after-init-hook 'org-roam-mode)

  (setq
   ;; removed in v2
   ;; org-roam-db-update-method 'immediate
   ;; org-roam-tag-sources '(prop vanilla all-directories)
   org-roam-completion-everywhere nil
   ;; org-roam-link-auto-replace t
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id

   ;; removed in v2
   ;; org-roam-buffer-width 0.15
   ;; org-roam-buffer-window-parameters '((side . right)
   ;;                                     (no-delete-other-windows . t)
   ;;                                     (mode-line-format . none))
   )

  ;; (with-eval-after-load 'org-roam-doctor
  ;;  (dolist (p org-roam-doctor--supported-roam-properties)
  ;;    (add-to-list 'org-default-properties p)))

  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new
           (file+head
            "${slug}.org"
            "\n#+title: ${title}\n")
           :unnarrowed t
           :jump-to-captured t))
        org-roam-capture-ref-templates
        '(("r" "ref" plain "%?" :if-new
           (file+head "${slug}.org" "#+title: ${title}\n[[id:A7F4AA20-A247-43D2-BCBC-2ED6108AF344][UNREAD]]\n\n%(zp/org-protocol-insert-selection-dwim \"${body}\")")
           :unnarrowed t
           :jump-to-captured t
           )
          ("n" "non-browser" plain "%?" :if-new
           (file+head "read-later/${slug}.org" "#+title: ${title}\n%(zp/org-protocol-insert-selection-dwim \"${body}\")")
           :unnarrowed t)
          ;; ("p" "pocket" plain
          ;;  (function org-roam-capture--get-point)
          ;;  ""
          ;;  :file-name "read-later/${slug}"
          ;;  :head ":PROPERTIES:\n:roam_key: ${ref}\n:created: %u\n:END:\n[[roam:web]]\n#+titls: ${title}\n${body}"
          ;;  :unnarrowed t)
          )
        org-roam-dailies-capture-templates
        '(("w" "work" plain
           "%(org-roam-capture--add-link-to \"work\") %?"
           :if-new
           (file+head+olp
            "%<%Y-%m-%d %a>.org"
            "#+title: %<%Y-%m-%d %a>"
            ("Work"))
           :jump-to-captured t
           :unnarrowed t)
          ("j" "journal" plain
           "%(org-roam-capture--add-link-to \"journal\") %?"
           :if-new
           (file+head+olp
            "%<%Y-%m-%d %a>.org"
            "#+title: %<%Y-%m-%d %a>"
            ("Journal"))
           :jump-to-captured t
           :unnarrowed t)))

  ;; org-roam-buffer-window-parameters get reset on Layout/Workspace change,
  ;; they should be respected
  ;; (add-hook 'persp-activated-functions #'org-roam--reset-roam-buffer-window)
  ;; (add-hook 'eyebrowse-post-window-switch-hook #'org-roam--reset-roam-buffer-window)

  (with-eval-after-load 'org-capture
    (setq org-capture-bookmark nil)
    (define-key org-capture-mode-map (kbd "r") nil)
    (define-key magit-section-mode-map (kbd "SPC") nil)
    ;; (spacemacs/declare-prefix-for-minor-mode 'spacemacs-org-capture-mode "r" "org-roam")
    ;; (spacemacs/declare-prefix-for-minor-mode 'spacemacs-org-capture-mode "rt" "org-roam-tags")
    (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
      ;; "rb" 'org-roam-switch-to-buffer
      "rf" #'org-roam-node-find
      "ri" #'org-roam-node-insert
      "rl" #'org-roam-buffer-toggle
      "rta" #'org-roam-tag-add
      "rtd" #'org-roam-tag-delete))

  (with-eval-after-load 'persp-mode
    ;; if I don't do this with a timer, the Org layer persp-post-init logic
    ;; always runs after and resets it
    (run-at-time
     "0.5" nil
     (lambda ()
       (spacemacs|define-custom-layout "@Org"
         :binding "o"
         :body
         (progn
           (find-file (concat org-roam-directory "/index.org"))
           ;; (org-roam-buffer-activate)
           )))))
  (with-eval-after-load 'org-roam
    (org-roam-db-autosync-mode)
    (add-to-list 'display-buffer-alist
                 '(("\\*org-roam\\*"
                    (display-buffer-in-direction)
                    (direction . right)
                    (window-width . 0.25)
                    (window-height . fit-window-to-buffer))))))

(defun ag-org/init-org-roam-ui ()
  (use-package org-roam-ui
    :after org-roam
    :hook (spacemacs-post-user-config . org-roam-ui-mode)
    :config
    (setq org-roam-ui-port 8081
          org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start nil)))

(defun ag-org/post-init-org-ref ()
  (setq org-ref-default-bibliography '("~/SyncMobile/Papers/references.bib")
        org-ref-pdf-directory "~/SyncMobile/Papers/"
        org-ref-bibliography-notes "~/SyncMobile/Papers/notes.org"))

(defun ag-org/init-org-edit-indirect ()
  (use-package org-edit-indirect
    :after (org edit-indirect)))

(defun ag-org/post-init-expand-region ()
  (use-package expand-region
    :after org
    :config

    (defun er/add-org-mode-expansions* ()
      "Adds org-specific expansions for buffers in org-mode"
      (set (make-local-variable 'er/try-expand-list)
           (append
            (remove #'er/mark-defun er/try-expand-list)
            '(er/mark-word
              er/mark-between
              er/mark-symbol
              er/mark-symbol-with-prefix
              er/mark-inside-pairs
              org-mark-subtree
              er/mark-org-element
              er/mark-org-element-parent
              er/mark-org-code-block
              er/mark-sentence
              er/mark-org-parent
              er/mark-paragraph)))
      (set (make-local-variable 'er/save-mode-excursion)
           #'er/save-org-mode-excursion))

    (remove-hook 'org-mode-hook #'er/add-org-mode-expansions)
    (er/enable-mode-expansions 'org-mode #'er/add-org-mode-expansions*)))

;;; packages.el ends here
