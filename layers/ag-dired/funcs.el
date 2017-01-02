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

