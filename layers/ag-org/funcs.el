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
  (hs-alert "task done")
  (pomodoro/modify-menu-item "green"))

(defun pomodoro/on-break-over-hook ()
  (hs-alert "break over")
  (pomodoro/remove-menu-item))

(defun pomodoro/on-killed-hook ()
  (hs-alert "killed")
  (pomodoro/remove-menu-item))

(defun pomodoro/on-started-hook ()
  (hs-alert "- start churning -")
  (pomodoro/create-menu-item "red"))
