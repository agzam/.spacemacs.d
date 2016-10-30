(defconst ag-org-packages '((org :location built-in)
                            org-pomodoro
                            ox-reveal))

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
     
     ort/prefix-arg-directory "~/org"
     org-agenda-files '("~/org/tasks.org")

     ;; for Chrome layer
     edit-server-default-major-mode 'org-mode)

    ;; To save the clock history across Emacs sessions, use
    (org-clock-persistence-insinuate)

    ;; (spacemacs/toggle-mode-line-org-clock-on)

    (add-hook 'org-mode-hook 'flyspell-mode)

    (defun insert-current-date (arg)
      (interactive "P")
      (insert (if arg
                  (format-time-string "%d.%m.%Y")
                (format-time-string "%Y-%m-%d"))))))

(defun ag-org/post-init-ox-reveal ()
  (use-package ox-reveal
    :config
    (progn
      (setq org-reveal-title-slide nil))))

(defun ag-org/post-init-org-pomodoro ()
  (with-eval-after-load 'org-pomodoro
    (defun pomodoro/modify-menu-item (color)
      "color can be \"red\" \"green\" or \"yellow\""
      (let* ((hs (executable-find "hs"))
             (task-name (symbol-value 'org-clock-current-task))
             (cmd (concat " txt = hs.styledtext.new(\""
                          task-name
                          "\",{ color = hs.drawing.color.hammerspoon.osx_" color " });"
                          "globalMenubarItem = hs.menubar.newWithPriority(0);"
                          "globalMenubarItem:setTitle(txt)")))
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

    (add-hook 'org-pomodoro-finished-hook (lambda ()
                                            (progn
                                              (hs-alert "task done")
                                              (pomodoro/remove-menu-item)
                                              (pomodoro/modify-menu-item "green"))))

    (add-hook 'org-pomodoro-break-finished-hook (lambda ()
                                                  (hs-alert "break over")
                                                  (pomodoro/remove-menu-item)))
    (add-hook 'org-pomodoro-killed-hook (lambda ()
                                          (hs-alert "killed")
                                          (pomodoro/remove-menu-item)))
    (add-hook 'org-pomodoro-started-hook (lambda ()
                                           (hs-alert "- start churning -")
                                           (pomodoro/modify-menu-item "red"))))) 


