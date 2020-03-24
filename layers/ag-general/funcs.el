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
  "Switch to previous Spacemacs layout by briefly flashing layouts panel - so user can see where they're going"
  (interactive)
  (spacemacs/layouts-transient-state/persp-prev)
  (run-at-time "1 sec" nil #'spacemacs/layouts-transient-state/nil))

(defun spacemacs/persp-go-next ()
  "Switch to next Spacemacs layout by briefly flashing layouts panel - so user can see where they're going"
  (interactive)
  (spacemacs/layouts-transient-state/persp-next)
  (run-at-time "1 sec" nil #'spacemacs/layouts-transient-state/nil))
;;; funcs.el ends here

(defun diff-last-two-kills ()
  "Write the last two kills to temporary files and diff them."
  (interactive)
  (let ((old "/tmp/old-kill") (new "/tmp/new-kill"))
    (with-temp-file new
      (insert (current-kill 0 t)))
    (with-temp-file old
      (insert (current-kill 1 t)))
    (diff old new "-u" t)))

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
         (x (first geom))
         (y (second geom))
         (display-height (first (last geom))))
    (if on?
        (progn
          (set-frame-parameter frame 'undecorated nil)
          (toggle-frame-maximized))
      (progn
        (set-frame-position frame x y)
        (set-frame-parameter frame 'fullscreen 'maximized)
        (set-frame-parameter frame 'undecorated t)
        (set-frame-height frame (- display-height 26) nil t)
        (set-frame-position frame x y)))))

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
