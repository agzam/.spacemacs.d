(defun insert-current-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun pomodoro/create-menu-item (color)
  "color can be \"red\" \"green\" or \"yellow\""
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
                  (concat "-c" cmd))))

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
                  (concat "-c" cmd))))

(defun pomodoro/remove-menu-item ()
  "removes currently set pomodoro menu item"
  (let* ((hs (executable-find "hs"))
         (cmd " globalMenubarItem:delete(); globalMenubarItem = nil"))
    (call-process hs
                  nil 0 nil
                  (concat "-c" cmd))))

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

(defun ag/add-days-to-ifttt-date (dt days)
  "Takes datetime in IFTTT format e.g. `February 23, 2017 at 11:00AM`,
   turns it into emacs-lisp datetime
   and adds given number of days"
  (-some-> dt
           (substring 0 -2)
           (split-string  " " nil ",")
           ((lambda (x) (cons (car (cdr x)) (cons (car x) (cdr (cdr x))))))
           ((lambda (x) (mapconcat 'identity x " ")))
           (date-to-time)
           (time-add (days-to-time days))))

(defun ag/indent-org-entry ()
  "Indents current org entry"
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
