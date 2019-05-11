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

(defun hs-alert (message)
  "shows Hammerspoon's hs.alert popup with a MESSAGE"
  (when (and message (eq system-type 'darwin))
    (call-process
     (executable-find "hs")
     nil 0 nil "-c" (concat "hs.alert.show(\"" message "\", 1)"))))

(defun ag/atomic-edit-start ()
  (remove-hook 'markdown-mode-hook 'spacemacs/activate-mmm-mode)
  (remove-hook 'markdown-mode-hook 'spacemacs//cleanup-org-tables-on-save))

(defun ag/atomic-edit-done ()
  (kill-new (buffer-string) t)
  (when (eq system-type 'darwin)
    (shell-command "open -a \"Google Chrome\"")))

(defun ag/fix-frame ()
  "Toggle fullscreen off and on. OS X workaround."
  (when (spacemacs/toggle-fullscreen-frame-status)
    (spacemacs/toggle-fullscreen-frame-off)
    (spacemacs/toggle-fullscreen-frame-on)))

(defun ag/move-frame-one-display (direction)
  "Moves current frame to a monitor at given DIRECTION

DIRECTION - can be North, South, West, East"
  (when (eq system-type 'darwin)
    (let* ((hs (executable-find "hs"))
           (cmd (concat "hs.window.focusedWindow():moveOneScreen" direction "()")))
      (call-process hs nil 0 nil "-c" cmd)
      (ag/fix-frame))))

(spacemacs/transient-state-register-add-bindings 'zoom-frm
  '(("k" zoom-frm-in)
    ("j" zoom-frm-out)
    ("r" zoom-frm-unzoom)
    ("f" spacemacs/toggle-frame-fullscreen-non-native)
    ("m" spacemacs/toggle-maximize-frame)
    ("n" (ag/move-frame-one-display "North"))
    ("p" (ag/move-frame-one-display "South"))))

;; remove visual marks overlay after marks are deleted
(advice-add 'evil-delete-marks :after (lambda (&rest args) (evil-visual-mark-render)))

(defun ag/switch-to-app (pid)
  "Using third party tools tries to switch to the app with the given PID"
  (when (and pid (eq system-type 'darwin))
    (call-process (executable-find "hs") nil 0 nil "-c"
                  (concat "require(\"emacs\").switchToApp (\"" pid "\")"))))

(defvar ag/edit-with-emacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'ag/finish-edit-with-emacs)
    (define-key map (kbd "C-c C-k") 'ag/cancel-edit-with-emacs)
    map))

(define-minor-mode ag/edit-with-emacs-mode
  "Minor mode enabled on buffers opened by ag/edit-by-emacs"
  :init-value nil
  :lighter " editwithemacs"
  :keymap ag/edit-with-emacs-mode-map)

(defun ag/edit-with-emacs (&optional pid title screen)
  "Edit anything with Emacs

PID is a pid of the app (the caller is responsible to set that right)
TITLE is a title of the window (the caller is responsible to set that right)"
  (setq systemwide-edit-previous-app-pid pid)
  (select-frame-by-name "edit")
  (set-frame-position nil 400 400)
  (set-frame-size nil 800 600 t)
  (let ((buffer (get-buffer-create (concat "*edit-with-emacs " title " *"))))
    (set-buffer-major-mode buffer)
    (unless (bound-and-true-p global-edit-with-emacs-mode)
      (global-edit-with-emacs-mode 1))
    (with-current-buffer buffer
      (spacemacs/copy-clipboard-to-whole-buffer)
      (spacemacs/evil-search-clear-highlight)
      (delete-other-windows)
      (spacemacs/toggle-visual-line-navigation-on)
      (markdown-mode)
      (ag/edit-with-emacs-mode 1)
      (evil-insert 1))
    (switch-to-buffer buffer))
  (when (and pid (eq system-type 'darwin))
    (call-process
     (executable-find "hs") nil 0 nil "-c"
     (concat "require(\"emacs\").editWithEmacsCallback(\""
             pid "\",\"" title "\",\"" screen "\")"))))

(defun ag/turn-on-edit-with-emacs-mode ()
  "Turn on `ag/edit-with-emacs-mode' if the buffer derives from that mode"
  (when (string-match-p "*edit-with-emacs" (buffer-name (current-buffer)))
    (ag/edit-with-emacs-mode t)))

(define-global-minor-mode global-edit-with-emacs-mode
  ag/edit-with-emacs-mode ag/turn-on-edit-with-emacs-mode)

(defvar systemwide-edit-previous-app-pid nil
  "Last app that invokes `ag/edit-with-emacs'.")

(defun ag/finish-edit-with-emacs ()
  (interactive)
  (spacemacs/copy-whole-buffer-to-clipboard)
  (kill-buffer)
  (delete-frame)
  (call-process (executable-find "hs") nil 0 nil "-c"
                (concat "require(\"emacs\").switchToAppAndPasteFromClipboard (\"" systemwide-edit-previous-app-pid "\")"))
  (setq systemwide-edit-previous-app-pid nil))

(defun ag/cancel-edit-with-emacs ()
  (interactive)
  (kill-buffer)
  (delete-frame)
  (ag/switch-to-app systemwide-edit-previous-app-pid)
  (setq systemwide-edit-previous-app-pid nil))

(defun shruggie ()
  (interactive)
  (insert "¯\\_(ツ)_/¯"))

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
