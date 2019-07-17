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

(defun ag/atomic-edit-start ()
  (remove-hook 'markdown-mode-hook 'spacemacs/activate-mmm-mode)
  (remove-hook 'markdown-mode-hook 'spacemacs//cleanup-org-tables-on-save))

(defun ag/atomic-edit-done ()
  (kill-new (buffer-string) t)
  (when (eq system-type 'darwin)
    (shell-command "open -a \"Google Chrome\"")))

;; remove visual marks overlay after marks are deleted
(advice-add 'evil-delete-marks :after (lambda (&rest args) (evil-visual-mark-render)))

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
