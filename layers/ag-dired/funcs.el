;;; funcs.el --- ag-dired layer functions file.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun buffer-with-dired-item ()
  "Creates buffer with current item of dired or direx buffer"
  (cond ((eq major-mode 'direx:direx-mode) (-> (direx:item-at-point!)
                                               direx:item-tree
                                               direx:file-full-name
                                               find-file-noselect))
        ((eq major-mode 'dired-mode) (find-file-noselect (dired-get-file-for-visit)))))

(defun ag/dired-find-file-other-window* (buffer direction)
  "internal function used by `dired-find-file-other-window`"
  (interactive)
  (let ((window
         (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction direction))
          (t
           (split-window (selected-window) nil direction t)))))
    (window--display-buffer buffer window 'window)
    (select-window window)))

(spacemacs|define-transient-state dired-open-item-other-window
  :title "Open item in other window"
  :doc
  "\n[_j_/_k_] down/up [_h_/_l_] left/right [_q_] quit"
  :bindings
  ("j" (ag/dired-find-file-other-window* (buffer-with-dired-item) 'below) :exit t)
  ("k" (ag/dired-find-file-other-window* (buffer-with-dired-item) 'above) :exit t)
  ("h" (ag/dired-find-file-other-window* (buffer-with-dired-item) 'left) :exit t)
  ("l" (ag/dired-find-file-other-window* (buffer-with-dired-item) 'right) :exit t)
  ("q" nil :exit t))

(defun eshell-cwd ()
  " Sets the eshell directory to the current buffer"
  (interactive)
  (let ((path (file-name-directory (or (buffer-file-name) default-directory))))
    (with-current-buffer "*eshell*"
      (cd path)
      (eshell-reset))))

(defun ffap-file-at-point-with-spaces ()
  "Detects a filename in `ls' output in an arbitrary buffer.

Correctly handles filenames with whitespaces - something that `ffap-file-at-point' apparently cannot"
  (let ((start (save-excursion (re-search-backward "\s\s\\|^")))
        (end (save-excursion (re-search-forward "\s\s\\|$"))))
    (string-trim
     (buffer-substring-no-properties start end))))

(defun eshell-action-on-file-or-dir-at-point (arg)
  "If thing-at-point is:
- file -  opens it in a buffer (same buffer)
- directory - `cd`s to it

With prefix argument opens things in the other-window"
  (interactive "P")
  (let ((item (ffap-file-at-point-with-spaces)))
    (if arg
        (when (file-exists-p item) (find-file-other-window item))
      (if (file-directory-p item)
          (with-current-buffer (current-buffer)
            (cd (expand-file-name item))
            (eshell-reset))
        (when (file-exists-p item) (find-file item))))))

(defun eshell-action-on-file-or-dir-at-point-other-window ()
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'eshell-action-on-file-or-dir-at-point)))

(defun direx:jump-to-project-root-or-current-dir ()
  "Open in Direx - project root if there's one, otherwise current directory."
  (interactive)
  (let ((buf (direx-project:find-project-root-noselect
              (or buffer-file-name default-directory))))
    (if buf
        (progn
          (direx:maybe-goto-current-buffer-item buf)
          (switch-to-buffer buf))
      (direx:find-directory "."))))

;;; funcs.el ends here
