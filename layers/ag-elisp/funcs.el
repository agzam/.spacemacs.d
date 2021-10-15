;; -*- lexical-binding: t -*-

(defun aget-in (alist &rest keys)
  "Recursively find KEYs in ALIST.

Example: (aget-in books 'details 'author))."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

;; override the default function with the one that works with chemacs
(defun ag/find-user-init-file ()
  "Edit the `user-init-file', in the current window."
  (interactive)
  (find-file-existing
   (expand-file-name "init.el" user-emacs-directory)))
