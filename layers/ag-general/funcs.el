;; -*- mode: emacs-lisp; lexical-binding: t -*-
;;; funcs.el --- ag-general layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun ag/region-or-word-at-point-str ()
  "Returns string of selected region or word at point"
  (let* ((bds (if (use-region-p)
                  (cons (region-beginning) (region-end))
                (bounds-of-thing-at-point 'word)))
         (p1 (car bds))
         (p2 (cdr bds)))
    (buffer-substring-no-properties p1 p2)))

(defun aget-in (alist &rest keys)
  "Recursively find KEYs in ALIST.

Example: (aget-in books 'details 'author))."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun notify-osx (title message)
  (when (eq system-type 'darwin)
    (call-process (executable-find "terminal-notifier")
                  nil 0 nil
                  "-group" "Emacs"
                  "-title" title
                  "-sender" "org.gnu.Emacs"
                  "-message" message)))

(defun notify-send (summary &optional body options)
  "Create notification using libnotify in Linux.

can take notify-send's OPTIONS along with SUMMARY and BODY
Example: `(notify-send \"org-pomodoro\" \"Break is over, get back to work\" '(category \"break-over\"))`
OPTIONS can include '(urgency expire-time app-name icon category hint), refer to `notify-send --help' for details"
  (cond
   ((eq system-type 'gnu/linux)
    (cl-flet ((get-prop (lambda (k)
                          (let ((p (plist-get options k)))
                            (when p (list (concat "--" (symbol-name k)) p))))))
      (let* ((opts '(urgency expire-time app-name icon category hint))
             (args (append (list
                            (executable-find "notify-send")
                            nil 0 nil
                            summary (or body ""))
                           (-flatten (mapcar #'get-prop opts)))))
        (apply #'call-process args))))))

;; remove visual marks overlay after marks are deleted
(advice-add 'evil-delete-marks :after (lambda (&rest args) (evil-visual-mark-render)))

(defun shruggie (&optional do-not-escape?)
  (interactive "P")
  (if do-not-escape?
      (insert "¯\\_(ツ)_/¯")
    (insert "¯\\\\\\_(ツ)_/¯")))

(defun spacemacs/persp-go-prev ()
  "Switch to previous Spacemacs layout by briefly flashing transient panel, so user can see where they're going"
  (interactive)
  (spacemacs/layouts-transient-state/persp-prev)
  (run-at-time "1 sec" nil #'spacemacs/layouts-transient-state/nil))

(defun spacemacs/persp-go-next ()
  "Switch to next Spacemacs layout by briefly flashing transient panel, so user can see where they're going"
  (interactive)
  (spacemacs/layouts-transient-state/persp-next)
  (run-at-time "1 sec" nil #'spacemacs/layouts-transient-state/nil))

(defun spacemacs/eyebrowse-go-prev (&optional delete-current?)
  "Switch to previous Workspace by briefly flashing the transient panel, so user can see where they're going"
  (interactive "P")
  (if delete-current?
      (progn
        (eyebrowse-close-window-config)
        (message (format "workspace %s removed" (number-to-string (eyebrowse--get 'last-slot)))))
    (spacemacs/workspaces-transient-state/eyebrowse-prev-window-config))
  (run-at-time "1 sec" nil #'spacemacs/workspaces-transient-state/nil))

(defun spacemacs/eyebrowse-go-next (&optional create-new?)
  "Switch to next Workspace by briefly flashing the transient panel, so user can see where they're going"
  (interactive "P")
  (if create-new?
      (let ((cur-buffer (current-buffer)))
        (spacemacs/workspaces-transient-state/eyebrowse-create-window-config)
        (switch-to-buffer cur-buffer))
    (spacemacs/workspaces-transient-state/eyebrowse-next-window-config))
  (run-at-time "1 sec" nil #'spacemacs/workspaces-transient-state/nil))

(defun diff-last-two-kills (&optional ediff?)
  "Diff last couple of things in the kill-ring. With prefix open ediff."
  (interactive "P")
  (let* ((old "/tmp/old-kill")
         (new "/tmp/new-kill")
         (prev-ediff-quit-hook ediff-quit-hook))
    (cl-flet ((kill-temps
               ()
               (dolist (f (list old new))
                 (kill-buffer (find-buffer-visiting f)))
               (setq ediff-quit-hook prev-ediff-quit-hook)))
      (with-temp-file new
        (insert (current-kill 0 t)))
      (with-temp-file old
        (insert (current-kill 1 t)))
      (if ediff?
          (progn
            (add-hook 'ediff-quit-hook #'kill-temps)
            (ediff old new))
        (diff old new "-u" t)))))

(defun diff-buffers (buffer-A buffer-B)
  "Diff two buffers."
  (interactive
   (let* ((only-two? (eq 2 (count-windows)))
          (wins (sort (window-list)
                      (lambda (a b) (< (window-use-time a)
                                       (window-use-time b)))))
          (b1 (if only-two?
                  (window-buffer (first wins))
                (read-buffer "Buffer A to compare")))
          (b2 (if only-two?
                  (window-buffer (second wins))
                (read-buffer "Buffer B to compare"))))
     (list b1 b2)))
  (let ((old "/tmp/old-diff")
        (new "/tmp/new-diff"))
    (with-temp-file new
      (insert-buffer-substring buffer-A))
    (with-temp-file old
      (insert-buffer-substring buffer-B))
    (diff old new "-u" t)))

(defun toggle-frame-maximized-undecorated ()
  (interactive)
  (let* ((frame (selected-frame))
         (on? (and (frame-parameter frame 'undecorated)
                   (eq (frame-parameter frame 'fullscreen) 'maximized)))
         (geom (frame-monitor-attribute 'geometry))
         (x (nth 0 geom))
         (y (nth 1 geom))
         (display-height (nth 3 geom))
         (display-width (nth 2 geom))
         (cut (if on?
                  (if ns-auto-hide-menu-bar 26 50)
                (if ns-auto-hide-menu-bar 4 26))))
    (set-frame-position frame x y)
    (set-frame-parameter frame 'fullscreen-restore 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter frame 'undecorated (not on?))
    (set-frame-height frame (- display-height cut) nil t)
    (set-frame-width frame (- display-width 20) nil t)
    (set-frame-position frame x y)))

(defun reinforce-frame-full-width-height ()
  "Set full-width and full-height frame parameters based on
actual pixel values of frame geometry."
  (let* ((geom (frame-monitor-attribute 'geometry))
         (width (nth 2 geom))     ;
         (height (first (last geom))))
    (print width)
    (set-frame-size (selected-frame) (- width 18) height t)
    ;; (when (< (- width (frame-outer-width)) 20)
    ;;   (set-frame-parameter (selected-frame) 'full-width t))
    ;; (when (< (- height (frame-outer-height)) 20)
    ;;   (set-frame-parameter (selected-frame) 'full-height t))
    ))

;; TODO: remove when https://github.com/abo-abo/swiper/issues/2454 fixed
(defun counsel-company ()
  "Complete using `company-candidates'."
  (interactive)
  (company-mode 1)
  (unless company-candidates
    (company-complete))
  (let ((len (cond (company-prefix
                    (length company-prefix))
                   (company-common
                    (length company-common)))))
    (when len
      (setq ivy-completion-beg (- (point) len))
      (setq ivy-completion-end (point))
      (ivy-read "Candidate: " company-candidates
                :action #'ivy-completion-in-region-action
                :caller 'counsel-company))))

(defun heroku-config (app &optional key)
  "Return config value(s) for given APP as json object. If KEY
provided, returns its value"
  (with-temp-buffer
    (let* ((raw (progn
                  (call-process
                   (executable-find "heroku")
                   nil (current-buffer) nil
                   "config"
                   "--app" app
                   "--json")
                  (buffer-string)))
           (parsed (json-read-from-string raw)))
      (if key
          (alist-get key parsed)
        parsed))))

(defvar pg-app-connections nil "PostgreSQL connections")

(defun pg-app-connections ()
  (if pg-app-connections pg-app-connections
    (progn
      (setq pg-app-connections `(("local" . "postgres://cc_user:password@localhost/cc")
                                 ("dev" . ,(heroku-config "crawlingchaos-dev" 'DATABASE_URL))
                                 ("staging" . ,(heroku-config "crawlingchaos-staging" 'DATABASE_URL))))
      pg-app-connections)))

(defun pg-app (app)
  (interactive (list (completing-read "Choose: " (mapcar 'car (pg-app-connections)))))
  (let* ((url (url-generic-parse-url (alist-get app (pg-app-connections) nil nil #'string=)))
         (sql-database (replace-regexp-in-string "\\/" "" (url-filename url)))
         (sql-server (url-host url))
         (sql-user (url-user url)))
    (setenv "PGPASSWORD" (url-password url))
    (sql-postgres app)))

(defun remove-from-shell-history (str)
  "Find given STR in bash_history file and remove all occurences of it"
  (let ((hist-file (getenv "HISTFILE")))
    (with-temp-buffer
      (insert-file-contents-literally hist-file)
      (let ((history-list (seq-mapcat (lambda (x) (concat x "\n"))
                                      (seq-remove
                                       (lambda (x)
                                         (when (string= x str)
                                           (print (concat "removing" str)))
                                         (string= x str))
                                       (split-string (buffer-string) "\n" t))
                                      'string)))
        (erase-buffer)
        (insert history-list)
        (write-file hist-file)))))

(defun shell-history ()
  "Ivy prompt for bash_history"
  (interactive)
  (let ((history-list (with-temp-buffer
                        (insert-file-contents-literally (getenv "HISTFILE"))
                        (->
                         (buffer-string)
                         (split-string "\n" t)
                         (delete-duplicates :test #'string=)))))
    (ivy-read "Command: " history-list
              :action '(1
                        ("o" insert "insert")
                        ("d" remove-from-shell-history "delete")))))

;; override the default function with the one that works with chemacs
(defun spacemacs/find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing
   (expand-file-name "init.el" user-emacs-directory)))

(defun er/mark-line ()
  "Marks entire 'logical' line."
  (interactive)
  (evil-end-of-line)
  (set-mark (point))
  (evil-first-non-blank))

(defun mark-between (&optional inclusive?)
  "Mark between various delimeters within same line.
   With INCLUSIVE? marks with delimiters."
  (interactive)
  (let* ((pairs '(("/" "/") ("=" "=") ("~" "~") ("(" ")") ("\\[" "\\]") ("<" ">") ("'" "'") ("\"" "\""))))
    (dolist (pair pairs)
      (let* ((prev (point))
             (reg (ignore-errors (evil-select-paren
                                 (car pair) (cadr pair)
                                 nil
                                 nil
                                 nil 1
                                 inclusive?))))
        (when (and reg
                   (<= (line-beginning-position) (car reg))
                   (<= (nth 1 reg) (line-end-position)))
          (deactivate-mark t)
          (goto-char (nth 1 reg))
          (set-mark (point))
          (goto-char (car reg))
          (return reg))))))

;;; funcs.el ends here
