;; -*- mode: emacs-lisp; lexical-binding: t -*-
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

(defun xwidget-webkit-get-url-buffer (url)
  "Returns xwidget buffer that points to URL, nil if none."
  (interactive)
  (when-let* ((r (lambda (x) (replace-regexp-in-string "http\\(s\\|\\)://" "" x)))
              (fnd (seq-find
                    (lambda (x)
                      (string= (concat (funcall r url) "/")
                               (funcall r (xwidget-webkit-uri x))))
                    xwidget-list)))
    (xwidget-buffer fnd)))

(defun xwidget-webkit-url-get-create (url &optional buffer-name)
  "Opens existing xwidget buffer, if it exists for the given URL,
or creates new session. Optionally, BUFFER-NAME can be set"
  (interactive (list (or (thing-at-point 'url)
                         (car (browse-url-interactive-arg "xwidget url: ")))))
  (require 'xwidget)
  (let ((lexical-binding t))
   (if-let ((buf (xwidget-webkit-get-url-buffer url)))
       (switch-to-buffer buf)
     (xwidget-webkit-new-session
      url
      (lambda (session _)
        (with-current-buffer (xwidget-buffer session)
          (rename-buffer (or buffer-name (concat "*xwidget " url "*")))))))))

(defun kill-xwidget-buffer ()
  (interactive)
  (kill-current-buffer)
  )


;;; funcs.el ends here
