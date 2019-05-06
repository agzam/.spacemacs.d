;;; funcs.el --- Org-mode functions
;;
;; Copyright (c) 2017 Ag Ibragimov
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Code:

(defun insert-current-date (arg)
  "Insert today's date.
Without universal ARG: 2017-11-08 With universal ARG: 08.11.2017"
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))


(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

Will prompt for timestamp if the entry has no timestamp.
If FILE is nil, refile in the current buffer."
  (interactive)
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil)))
         (date (org-date-to-gregorian datetree-date)))
    (save-excursion
      (with-current-buffer (current-buffer)
        (org-cut-subtree)
        (when file
          (find-file file))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)))))

(defun org-goto-datetree-date (&optional date)
  (interactive)
  (save-restriction
    (let* ((datetree-date (org-read-date))
           (dt (org-date-to-gregorian datetree-date)))
      (org-datetree-find-date-create dt t)
      (org-show-hidden-entry)
      (show-subtree))))

(defun org-goto-last-heading (&optional maxlevel)
  "Go to the last heading in the current subtree."
  (interactive "P")
  (if (listp maxlevel)
      (setq maxlevel 4)
    (unless maxlevel (setq maxlevel 3)))
  (setq currlevel 1)
  (while (<= currlevel maxlevel)
    (org-next-visible-heading 1)
    (if (not (org-at-heading-p))
        (progn
          (org-previous-visible-heading 1)
          (org-cycle)
          (setq currlevel (1+ currlevel))))))


(defun pomodoro/create-menu-item (color)
  "Create Hammerspoon `hs.menubar`.

item COLOR can be \"red\" \"green\" or \"yellow\"."
  (let* ((hs (executable-find "hs"))
         (task-name (symbol-value 'org-clock-current-task))
         (cmd (concat "if globalMenubarItem then globalMenubarItem:delete() end; "
                      "txt = hs.styledtext.new(\""
                      task-name
                      "\",{ color = hs.drawing.color.hammerspoon.osx_" color " });"
                      "globalMenubarItem = hs.menubar.newWithPriority(0);"
                      "globalMenubarItem:setTitle(txt)")))
    (call-process hs
                  nil 0 nil
                  "-c" cmd)))

(defun pomodoro/modify-menu-item (color)
  "Change COLOR of the OSX menu item, previously created by `pomodoro/create-menu-item'."
  (let* ((hs (executable-find "hs"))
         (cmd (concat "if globalMenubarItem then "
                      "txt = hs.styledtext.new(globalMenubarItem:title() "
                      ",{ color = hs.drawing.color.hammerspoon.osx_" color " });"
                      "globalMenubarItem:setTitle(txt);"
                      "end")))
    (message cmd)
    (call-process hs
                  nil 0 nil
                  "-c" cmd)))

(defun pomodoro/remove-menu-item ()
  "Remove previously set pomodoro item - `hs.menubar` item."
  (let* ((hs (executable-find "hs"))
         (cmd " globalMenubarItem:delete(); globalMenubarItem = nil"))
    (call-process hs
                  nil 0 nil
                  "-c" cmd)))

(defun pomodoro/on-finished-hook ()
  "When pomodoro is done."
  (cond
   ((eq system-type 'darwin)
    (progn
      (hs-alert "task done")
      (pomodoro/modify-menu-item "green")))
   ((eq system-type 'gnu/linux)
    (notify-send "org-pomodoro" "Task is done, good job!" '(category "finished")))))

(defun pomodoro/on-break-over-hook ()
  "When pomodoro break is over."
  (cond ((eq system-type 'darwin)
         (progn (hs-alert "break over")
                (pomodoro/remove-menu-item)))
        ((eq system-type 'gnu/linux)
         (notify-send "org-pomodoro" "Break is over, get back to work"
                      '(category "break-over")))))

(defun pomodoro/on-killed-hook ()
  "When you kill pomodoro."
  (cond ((eq system-type 'darwin)
         (progn (hs-alert "killed")
                (pomodoro/remove-menu-item)))
        ((eq system-type 'gnu/linux)
         (notify-send "org-pomodoro" "Pomodoro got killed"
                      '(category "killed")))))

(defun pomodoro/on-started-hook ()
  "When pomodoro starts."
  (cond
   ((eq system-type 'darwin)
    (progn
      (hs-alert "- start churning -")
      (pomodoro/create-menu-item "red")))
   ((eq system-type 'gnu/linux)
    (notify-send "org-pomodoro" "Let's get to it" '(category "started")))))

;;;; completion on Tab for `#+` stuff
(defun ag/org-mode-hook ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(defun ag/add-days-to-ifttt-date (datetime days)
  "Takes DATETIME in ifttt.com format e.g. `February 23, 2017 at 11:00AM` and turns it into emacs-lisp datetime and then adds given number of DAYS."
  (-some-> datetime
           (substring 0 -2)
           (split-string  " " nil ",")
           ((lambda (x) (cons (car (cdr x)) (cons (car x) (cdr (cdr x))))))
           ((lambda (x) (mapconcat 'identity x " ")))
           (date-to-time)
           (time-add (days-to-time days))
           ((lambda (x) (format-time-string "%Y-%m-%d" x)))))

(defun ag/indent-org-entry ()
  "Properly sets indentation for the current org entry."
  (interactive)
  (outline-show-entry)
  (forward-line 1)
  (set-mark-command nil)
  (org-end-of-subtree)
  (backward-char)
  (indent-region (region-beginning) (region-end) 2)
  (flush-lines "^$" nil nil t)
  (outline-hide-entry))

(defun ag/org-toggle-tags (tags)
  "Toggle TAGS on the current heading."
  (let ((existing (org-get-tags-at nil t)))
    (when tags
      (dolist (i tags)
        (when (not (member i existing))
          (org-toggle-tag i 'on))))))

(defun ag/org-url? ()
  "If org heading title is an org-link, it check if the link is 'broken' - contains line-breaks, then it fixes it."
  (let* ((title (org-entry-get (point) "ITEM"))
         (url? (string-match "\\[\\[.*" title))
         (proper-url? (string-match "\\[\\[.*\\]\\[.*\\]\]" title)))
    url?))

(defun ag/org-fix-heading-line-break (&optional pnt)
  "If org heading title is an org-link, it check if the link is 'broken' - contains line-breaks, then it fixes it."
  (let* ((title (org-entry-get (or pnt (point)) "ITEM"))
         (url? (string-match "\\[\\[.*" title))
         (proper-url? (string-match "\\[\\[.*\\]\\[.*\\]\]" title)))
    (if (and url? (not proper-url?))
        (if (re-search-forward "\n")
            (progn
              (replace-match " ")
              (ag/org-fix-heading-line-break))))))

(defun ag/--process-ifttt-pocket-entry ()
  "Process individually a single entry in org 'pocket'"
  (ag/org-fix-heading-line-break (point))
  (ag/indent-org-entry)
  (let* ((start (org-entry-beginning-position)))
    (let ((tags (-some-> (org-entry-get start "tag")
                         (split-string "," t "\s")))
          (deadline (org-entry-get start "DEADLINE"))
          (added-at (org-entry-get start "AddedAt")))
      ;; set the tags of the heading based on 'Tag' property
      (ag/org-toggle-tags tags)
      (when (and added-at (not deadline))
        (org--deadline-or-schedule nil 'deadline (ag/add-days-to-ifttt-date added-at 30)))
      (append-to-file (org-entry-beginning-position) (org-entry-end-position) "~/Dropbox/org/read-later.org")
      (delete-region (org-entry-beginning-position) (org-entry-end-position)))))

(defun ag/process-ifttt-pocket ()
  "Process org 'pocket' - entries imported via IFTTT"
  (let ((buf (find-file-noselect "~/Dropbox/org/pocket.txt")))
    (set-buffer buf)
    (org-map-entries (lambda ()
                       (ag/--process-ifttt-pocket-entry)
                       (setq org-map-continue-from (outline-previous-heading))) nil 'file)
    ;;; for whatever stupid reason, it would not process the last entry, I couldn't figure out how to fix that, so ended up scanning the list twice
    (org-map-entries 'ag/--process-ifttt-pocket-entry nil 'file)
    (save-buffer)
    (kill-buffer)))

(defun ag/set-tangled-file-permissions ()
  "Set specific file permissions after `org-babel-tangle'."
  (let ((fs-lst '(("~/.ssh/config" . #o600)
                  ("~/.ec" . #o700))))
    (dolist (el fs-lst)
      (when (file-exists-p (car el))
        (set-file-modes (car el) (cdr el))))))

(defun ag/org-meta-return (&optional ignore)
  "Context respecting org-insert."
  (interactive "P")
  (if ignore
      (org-return-indent)
    (cond
     ;; checkbox
     ((org-at-item-checkbox-p) (org-insert-todo-heading nil))
     ;; item
     ((org-at-item-p) (org-insert-item))
     ;; todo element
     ;; ((org-element-property :todo-keyword (org-element-context))
     ;;  (org-insert-todo-heading 4))
     ;; heading
     ((org-at-heading-p) (org-insert-heading-respect-content))
     ;; plain text item
     ((string-or-null-p (org-context))
      (progn
        (let ((org-list-use-circular-motion t))
          (org-beginning-of-item)
          (end-of-line)
          (ag/org-meta-return))))
     ;; fall-through case
     (t (org-return-indent)))))

;;;; System-wide org capture
(defvar systemwide-capture-previous-app-pid nil
  "Last app that invokes `activate-capture-frame'.")

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame."
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defun activate-capture-frame (&optional pid title keys)
  "Run ‘org-capture’ in capture frame.

PID is a pid of the app (the caller is responsible to set that right)
TITLE is a title of the window (the caller is responsible to set that right)
KEYS is a string associated with a template (will be passed to `org-capture')"
  (setq systemwide-capture-previous-app-pid pid)
  (select-frame-by-name "capture")
  (set-frame-position nil 400 400)
  (set-frame-size nil 1000 400 t)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture nil keys))

(defun convert-markdown-link-to-org ()
  (interactive)
  (when (markdown-link-p)
    (let* ((l (markdown-link-at-pos (point)))
           (desc (nth 2 l))
           (url (nth 3 l)))
      (markdown-kill-thing-at-point)
      (org-insert-link nil url desc))))

(defun convert-to-jira-link ()
  "Converts a word (simple number or a JIRA ticket with prefix) to a proper JIRA link."
  (interactive)
  (let* ((jira-base-url "https://jira.dividendsolar.com")
         (w (symbol-at-point))
         (bounds (bounds-of-thing-at-point 'symbol))
         (ticket (string-to-number (replace-regexp-in-string "[^0-9]" ""
                                                             (symbol-name w))))
         (prefix (replace-regexp-in-string "[0-9]" "" (symbol-name w)))
         (jira-project-prefix (if (string-empty-p prefix) "scrum-" prefix))
         (uri (concat
               jira-base-url
               "/browse/"
               (concat jira-project-prefix (number-to-string ticket))))
         (label (string-trim
                 (if (string-match-p jira-project-prefix (downcase (symbol-name w)))
                     (upcase (symbol-name w))
                   (upcase (concat jira-project-prefix (number-to-string ticket)))))))
    (save-excursion)
    (delete-region (car bounds) (cdr bounds))
    (cond ((eq major-mode 'org-mode)
           (insert (concat "[[" uri "][" label "]]")))

          ((eq major-mode 'markdown-mode)
           (insert (concat "[" label "](" uri ")")))

          ((string-match-p "COMMIT_EDITMSG" (or buffer-file-name ""))
           (insert (concat "[" label "]\n" uri)))

          (t (insert uri)))))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame."
  (when (and (equal "capture" (frame-parameter nil 'name))
             (not (eq this-command 'org-capture-refile)))
    (ag/switch-to-app systemwide-capture-previous-app-pid)
    (delete-frame)))

(defadvice org-capture-refile
    (after delete-capture-frame activate)
  "Advise ‘org-refile’ to close the frame."
  (delete-frame))

(defadvice user-error
    (before before-user-error activate)
  "Advice"
  (when (eq (buffer-name) "*Org Select*")
    (ag/switch-to-app systemwide-capture-previous-app-pid)))

(provide 'funcs)

;;; funcs.el ends here
