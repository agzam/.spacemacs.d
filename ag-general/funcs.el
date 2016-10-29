(defun get-default-height ()
  (+ 8 (/ (- (display-pixel-height) 120)
          (frame-char-height))))

(defun get-default-width ()
  (+ 13 (/ (- (display-pixel-width) 120)
           (frame-char-width))))

(defun max-frame () 
  "maximizing frame without borders"
  (interactive)
  (set-frame-position nil 0 -21)
  (set-frame-height (selected-frame) (get-default-height))
  (set-frame-width (selected-frame) (get-default-width)))

(defun ag/region-or-symbol-bounds ()
  (if (region-active-p)
      (cons (region-beginning)
            (region-end))
    (bounds-of-thing-at-point 'symbol)))

(defun ag/buffer-substring ()
  "grabs selected region's text, if any"
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (word-at-point)))

(defun ag/helm-google-suggest ()
  "Smarter helm-google-suggest - searches for selected text (if any)"
  (interactive)
  (let ((sub-str (ag/buffer-substring)))
    (if sub-str
        (helm-google-suggest-action sub-str)
      (helm-google-suggest))))

;;;;;;;;;;;;;;;;;;;
;;;; Org-mode ;;;;;
;;;;;;;;;;;;;;;;;;;
(defun insert-current-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun notify-osx (title message)
  (call-process (executable-find "terminal-notifier")
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" message))

(defun hs-alert (message)
  (when message
    (call-process (executable-find "hs")
                  nil 0 nil
                  (concat "-c" "hs.alert.show(\"" message "\", 1)"))))

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

(with-eval-after-load 'org-pomodoro
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
                                         (pomodoro/modify-menu-item "red"))))
;;;;;;;;;;;;;;;;;;;
;;;; Clojure ;;;;;;
;;;;;;;;;;;;;;;;;;;
(defun switch-to-nrepl-window ()
  (let* ((nrepl-buf (nrepl-make-buffer-name
                     nrepl-server-buffer-name-template
                     (clojure-project-dir (cider-current-dir)) nil nil -1)))
    (when (not (equal (buffer-name) nrepl-buf)) 
      (switch-to-buffer-other-window nrepl-buf))))

(with-eval-after-load 'cider
  (advice-add 'cider-jack-in :after (lambda (_) (save-selected-window (switch-to-nrepl-window))))

  (add-hook 'cider-connected-hook (lambda ()
                                    (save-selected-window
                                      (switch-to-nrepl-window)
                                      (split-window-below-and-focus)
                                      (switch-to-buffer (cider-current-repl-buffer))))))

(defun cljr-toggle-ignore-form ()
  "clojure - ignore (comment) form"
  (interactive)
  (if (search-backward "#_" 2 t 1)
      (delete-char 2)
    (progn
      (let ((fc (following-char)))
        (cond ((-contains? '( ?\) ?\] ?\} ) fc) (paredit-backward-up))
              ((-contains? '( ?\( ?\[ ?\: ?\{ ) fc) nil)
              (t (beginning-of-thing 'sexp)))
        (insert "#_")))))

(defun ag/clojars-find ()
  "Lookup for symbol at point on clojars. Useful for updating packages in project.clj"
  (interactive)
  (let ((s (symbol-at-point)))
    (when s (clojars s))))

(defun project-clj-hook ()
  "for project.clj files"
  (if (and (stringp buffer-file-name)
           (string-match "\\project.clj|build.boot\\'" buffer-file-name))
      (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "c" 'ag/clojars-find)
    (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "c" nil)))

;;(add-hook 'find-file-hook 'project-clj-hook)
(with-eval-after-load 'bind-map (add-hook 'find-file-hook 'project-clj-hook))
