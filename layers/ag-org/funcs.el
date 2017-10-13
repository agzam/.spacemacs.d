(defun insert-current-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun pomodoro/create-menu-item (color)
  "creates Hammerspoon `hs.menubar` item
COLOR can be \"red\" \"green\" or \"yellow\""
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
  "removes previously set pomodoro item - `hs.menubar` item"
  (let* ((hs (executable-find "hs"))
         (cmd " globalMenubarItem:delete(); globalMenubarItem = nil"))
    (call-process hs
                  nil 0 nil
                  "-c" cmd)))

(defun pomodoro/on-finished-hook ()
  (when (eq system-type 'darwin)
    (hs-alert "task done")
    (pomodoro/modify-menu-item "green")))

(defun pomodoro/on-break-over-hook ()
  (when (eq system-type 'darwin)
    (hs-alert "break over")
    (pomodoro/remove-menu-item)))

(defun pomodoro/on-killed-hook ()
  (when (eq system-type 'darwin)
    (hs-alert "killed")
    (pomodoro/remove-menu-item)))

(defun pomodoro/on-started-hook ()
  (when (eq system-type 'darwin)
    (hs-alert "- start churning -")
    (pomodoro/create-menu-item "red")))

;; completion on Tab for `#+` stuff
(defun ag/org-mode-hook ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(defun ag/add-days-to-ifttt-date (datetime days)
  "Takes DATETIME in ifttt.com format e.g. `February 23, 2017 at 11:00AM`,
   turns it into emacs-lisp datetime
   and adds given number of DAYS"
  (-some-> datetime
           (substring 0 -2)
           (split-string  " " nil ",")
           ((lambda (x) (cons (car (cdr x)) (cons (car x) (cdr (cdr x))))))
           ((lambda (x) (mapconcat 'identity x " ")))
           (date-to-time)
           (time-add (days-to-time days))))

(defun ag/indent-org-entry ()
  "Properly sets indentation for the current org entry"
  (interactive)
  (outline-show-entry)
  (forward-line 1)
  (set-mark-command nil)
  (org-end-of-subtree)
  (backward-char)
  (indent-region (region-beginning) (region-end) 2)
  (outline-hide-entry))

(defun ag/set-tags-and-schedules-for-ifttt-items ()
  "For org items imported via IFTTT, sets the right tags and specific
   deadline (added-at + number of days)"
  (progn
    (save-mark-and-excursion)
    (let ((tags (-some-> (org-entry-get (point) "tag")
                         (split-string "," t "\s")))
          (sched (org-entry-get (point) "DEADLINE"))
          (added-at (org-entry-get (point) "AddedAt"))
          (should-save? (not (buffer-modified-p))))
      (when tags
        (dolist (i tags)
          (when (not (member i (org-get-tags)))
            (org-toggle-tag i 'on)
            (org-set-tags (point) t))))
      (when (and added-at (not sched))
        (org--deadline-or-schedule nil 'deadline (ag/add-days-to-ifttt-date added-at 15)))
      (ag/indent-org-entry)
      (flush-lines "^$" nil nil t)
      (when should-save?
        (save-buffer)))))

(defun ag/set-tangled-file-permissions ()
  "set specific file permissions after files are tangled"
  (let ((fs-lst '(("~/.ssh/config" . #o600)
                  ("~/.ec" . #o700))))
    (dolist (el fs-lst)
      (when (file-exists-p (car el))
            (set-file-modes (car el) (cdr el))))))

(defun ag/org-meta-return (&optional ignore)
  "context respecting org-insert"
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

;;;;
;;;; System-wide org capture
;;;;
(defvar systemwide-capture-previous-app-pid nil
  "last app that invokes `activate-capture-frame'")

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defun activate-capture-frame (&optional pid title keys)
  "Run org-capture in capture frame

PID is a pid of the app (the caller is responsible to set that right)
TITLE is a title of the window (the caller is responsible to set that right)
KEYS is a string associated with a template (will be passed to `org-capture')"
  (setq systemwide-capture-previous-app-pid pid)
  (select-frame-by-name "capture")
  (set-frame-position nil 400 400)
  (set-frame-size nil 1000 400 t)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture nil keys))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (when (and (equal "capture" (frame-parameter nil 'name))
             (not (eq this-command 'org-capture-refile)))
    (ag/switch-to-app systemwide-capture-previous-app-pid)
    (delete-frame)))

(defadvice org-capture-refile
    (after delete-capture-frame activate)
  "Advise org-refile to close the frame"
  (delete-frame))

(defadvice user-error
    (before before-user-error activate)
  "Advice "
  (when (eq (buffer-name) "*Org Select*")
    (ag/switch-to-app systemwide-capture-previous-app-pid)))
