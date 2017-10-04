(defun ag/display-buffer-below (buffer alist)
  "internal function used by `dired-find-file-other-window`"
  (let ((window
         (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'below))
          (t
           (split-window (selected-window) nil 'below)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    (select-window window)))

(defun ag/display-buffer-above (buffer alist)
  "internal function used by `dired-find-file-other-window`"
  (let ((window
         (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'above))
          (t
           (split-window (selected-window) nil 'above)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    (select-window window)))

(defun ag/display-buffer-left (buffer alist)
  "internal function used by `dired-find-file-other-window`"
  (let ((window
         (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'left))
          (t
           (split-window (selected-window) nil 'left)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    (select-window window)))

(defun ag/display-buffer-right (buffer alist)
  "internal function used by `dired-find-file-other-window`"
  (let ((window
         (cond
          ((get-buffer-window buffer (selected-frame)))
          ((window-in-direction 'right))
          (t
           (split-window (selected-window) nil 'right)))))
    (window--display-buffer buffer window 'window alist display-buffer-mark-dedicated)
    (select-window window)))

(defun buffer-with-dired-item ()
  "Creates buffer with current item of dired or direx buffer"
  (cond ((eq major-mode 'direx:direx-mode) (-> (direx:item-at-point!)
                                               direx:item-tree
                                               direx:file-full-name
                                               find-file-noselect))
        ((eq major-mode 'dired-mode) (find-file-noselect (dired-get-file-for-visit)))))

(defun dired-find-file-other-window-above ()
  "Opens item in dired in other window above"
  (interactive)
  (ag/display-buffer-above (buffer-with-dired-item) nil))

(defun dired-find-file-other-window-below ()
  "Opens item in dired in other window below"
  (interactive)
  (ag/display-buffer-below (buffer-with-dired-item) nil))

(defun dired-find-file-other-window-left ()
  "Opens item in dired in other window on the left"
  (interactive)
  (ag/display-buffer-left (buffer-with-dired-item) nil))

(defun dired-find-file-other-window-right ()
  "Opens item in dired in other window on the right"
  (interactive)
  (ag/display-buffer-right (buffer-with-dired-item) nil))

(spacemacs|define-transient-state dired-open-item-other-window
  :title "Open item in other window"
  :doc
  "\n[_j_/_k_] down/up [_h_/_l_] left/right [_q_] quit"
  :bindings
  ("j" dired-find-file-other-window-below :exit t)
  ("k" dired-find-file-other-window-above :exit t)
  ("h" dired-find-file-other-window-left :exit t)
  ("l" dired-find-file-other-window-right :exit t)
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
