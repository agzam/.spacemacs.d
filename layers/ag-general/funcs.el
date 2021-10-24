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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; layouts & workspaces ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        (spacemacs/clone-workspace)
        (switch-to-buffer cur-buffer))
    (spacemacs/workspaces-transient-state/eyebrowse-next-window-config))
  (run-at-time "1 sec" nil #'spacemacs/workspaces-transient-state/nil))

(defun fasd-find-file-make-persp ()
  "Use fasd to open file or directory in a Spacemacs
layout (persp).

 If fasd item's project root already in a layout, switches to
that layout. If multiple layouts contain the same project root -
lets you choose one of them."
  (interactive)
  (let* ((lexical-binding t)
         (query (if fasd-enable-initial-prompt
                    (read-from-minibuffer "Fasd query: ")
                  ""))
         (results
          (split-string
           (shell-command-to-string
            (concat "fasd -l -R -a " query))
           "\n" t))
         (fpath (when results
                  ;; set `this-command' to `fasd-find-file' is required because
                  ;; `read-from-minibuffer' modifies its value, while `ivy-completing-read'
                  ;; assumes it to be its caller
                  (setq this-command 'fasd-find-file-make-persp)
                  (completing-read "Fasd query: " results nil t)))
         (proj-dir (projectile-project-root fpath))
         ;; find perspecitive with matching project-root
         (get-fname (lambda (buf)
                      (with-current-buffer buf
                        (cond ((eq major-mode 'dired-mode) (dired-get-filename))
                              (t (buffer-file-name))))))
         (persps (or (seq-filter
                      (lambda (p)
                        (when p
                          (->> p persp-buffers
                               (seq-map get-fname)
                               (seq-filter
                                (lambda (f)
                                  (string=
                                   proj-dir
                                   (projectile-project-root f)))))))
                      (persp-persps))
                     (let ((new-persp (persp-add-new
                                       (-> proj-dir directory-file-name file-name-nondirectory))))
                       (list new-persp))))
         (layout-name (if (< 1 (length persps))
                          (completing-read "Select layout " (seq-map 'persp-name persps))
                        (persp-name (car persps)))))
    (when layout-name
      (persp-switch layout-name)
      (find-file fpath)
      (delete-other-windows)
      (spacemacs/layouts-transient-state/body)
      (run-at-time "1.5 sec" nil #'spacemacs/layouts-transient-state/nil))))

(defun spacemacs/open-buffer-in-layout ()
  "Opens current buffer in a different layout.

Often you need to inspect a file from one project in a different
layout. It's cumbersome to have to manually switch to another
layout, then split windows and then choose the buffer from the
list of buffers. This adds ergonomics."
  (interactive)
  (let* ((cur-buf (current-buffer))
         (persp-names (->> (persp-persps)
                           (seq-map (lambda (p)
                                      (when p (persp-name p))))
                           (seq-remove (lambda (pname)
                                         (or (string= persp-last-persp-name pname)
                                             (null pname))))))
         (selected (completing-read "Select Layout: " persp-names)))
    (persp-switch selected)
    (split-window-horizontally)
    (switch-to-buffer cur-buf)
    (spacemacs/layouts-transient-state/body)
    (run-at-time "1 sec" nil #'spacemacs/layouts-transient-state/nil)))

;;;;;;;;;;;;;;;;;
;; Frame funcs ;;
;;;;;;;;;;;;;;;;;

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

(defun toggle-frame-full-height ()
  "Removes the title of the current frame and stretches it out to
  the display height. To be used on a Mac."
  (interactive)
  (if (frame-parameter nil 'undecorated)
      (set-frame-parameter nil 'undecorated nil)
    (progn
      (setq ns-auto-hide-menu-bar t)
      (setq ns-auto-hide-menu-bar nil)
      (set-frame-parameter nil 'undecorated t)
      (set-frame-position nil (car (frame-position)) 0)
      (set-frame-height nil (- (x-display-pixel-height) 29) nil :pixelwise))))

(defun center-frame-horizontally (&optional prompt percentage)
  "Positions the current frame in the middle of the screen,
vertically stretching it from top to bottom. Useful on ultra-wide monitor.
With universal argument prompts for the percentage - the horizontal screen estate the frame should occupy."
  (interactive "P")
  (let* ((stretch-ratio (string-to-number
                         (if prompt
                             (completing-read "Choose: " '("50%" "70%" "80%" "90%") nil t)
                           (number-to-string (or percentage 70)))))
         (x-pos (round (* (x-display-pixel-width) (* (/ (- 100 stretch-ratio) 2) 0.01))))
         (width (round (* (x-display-pixel-width) (* stretch-ratio 0.01)))))
    (set-frame-position nil x-pos 0)
    (set-frame-width nil width nil t)
    (when (not (frame-parameter nil 'undecorated))
      (toggle-frame-full-height))))

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

;;;;;;;;;;
;; Diff ;;
;;;;;;;;;;

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

(defun er/mark-line ()
  "Marks entire 'logical' line."
  (interactive)
  (evil-end-of-line)
  (set-mark (point))
  (evil-first-non-blank))

(defun er/mark-between (&optional inclusive?)
  "Mark between various delimeters within same line.
   With INCLUSIVE? marks with delimiters."
  (interactive)
  (require 'evil-common)
  (let* ((pairs '(("/" "/") ("=" "=") ("~" "~") ("(" ")") ("\\[" "\\]") ("<" ">") ("'" "'") ("\"" "\"") (" " " "))))
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
          (cl-return reg))))))

(defun ag/ivy-postframe--set-parameters ()
  (setq ivy-posframe-parameters
        `((alpha . 100)
          (undecorated . t)
          (left-fringe . 3)
          (right-fringe . 3)
          (internal-border-width . 1)
          (border-width . 1)
          (unsplittable . t)
          (background-color . ,(face-attribute 'default :background))
          (foreground-color . ,(face-attribute 'default :foreground)))
        ivy-posframe-width 160
        ivy-posframe-height 15
        ivy-posframe-hide-minibuffer nil))

(defun ag/posframe-poshandler-frame-bottom-left-corner (info)
  ;; somehow without this, ivy-posframe won't pick up current theme colors
  ;; even though for example which-key-posframe works fine
  (set-face-attribute 'fringe nil :background nil)
  ;; (ag/ivy-postframe--set-parameters)
  (cons 20 (- (plist-get info :parent-frame-height)
              (+ (plist-get info :posframe-height) 85))))

(defun ag/ivy-posframe-display-at-frame-bottom-left (str)
  (ivy-posframe--display str #'ag/posframe-poshandler-frame-bottom-left-corner))

(defun ag/posframe-arghandler (buffer-or-name arg-name value)
  (let ((info '(:lines-truncate t)))
    (or (plist-get info arg-name) value)))

(defun eval-last-sexp--around (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (not evil-move-beyond-eol)
           (or (evil-normal-state-p) (evil-motion-state-p)))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

;; in evil-mode cursor is at the sexp, so to target the last-sexp, it has to
;; move past the parentheses
(with-eval-after-load 'evil
 (unless evil-move-beyond-eol
   (advice-add 'eval-last-sexp :around 'eval-last-sexp--around)
   (advice-add 'eval-print-last-sexp :around 'eval-last-sexp--around)
   (advice-add 'pp-eval-last-sexp :around 'eval-last-sexp--around)

   (defun evil-forward-section-begin--after (_count)
     (recenter))

   (advice-add 'evil-forward-section-begin :after 'evil-forward-section-begin--after)))

;;; funcs.el ends here
