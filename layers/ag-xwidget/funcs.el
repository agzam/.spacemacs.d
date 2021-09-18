;;; funcs.el --- ag-xwidget layer functions
;;
;; Copyright (c) 2021 Ag Ibragimov
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Code:

(defun xwidget-webkit-url-get-create (url)
  "Opens existing xwidget buffer, if it exists for the given URL,
or creates new session."
  (interactive (list (or (thing-at-point 'url)
                         (car (browse-url-interactive-arg "xwidget url: ")))))
  (let* ((r (lambda (x) (replace-regexp-in-string "http\\(s\\|\\)://" "" x))))
    (if-let ((fnd (seq-find
                   (lambda (x)
                     ;; (print (concat (funcall r url) " =? "
                     ;;                (funcall r (xwidget-webkit-uri x))))
                     (string= (concat (funcall r url) "/")
                      (funcall r (xwidget-webkit-uri x))))
                   xwidget-list)))
        (display-buffer (xwidget-buffer fnd))
      (xwidget-webkit-browse-url url :new-session))))

(defun kill-xwidget-buffer ()
  (interactive)
  (kill-current-buffer)
  )


;;; funcs.el ends here
