(defun ag-mu4e/prepare-view ()
  (makunbound 'mu4e--prepare-view-timer)
  (setq mu4e--prepare-view-timer
        (run-at-time
         "0.05 sec" 0.05
         (lambda ()
           (when (and (not (get-buffer "*Article*"))
                      (boundp 'mu4e--prepare-view-timer))
             (cancel-timer mu4e--prepare-view-timer))
           (with-current-buffer "*Article*"
             (if (bound-and-true-p writeroom-mode)
                 (progn
                   (cancel-timer mu4e--prepare-view-timer)
                   (makunbound 'mu4e--prepare-view-timer))
               (progn
                 (setq writeroom-fullscreen-effect (frame-parameter (selected-frame) 'fullscreen))
                 (cl-letf ((writeroom-maximize-window nil)
                           (writeroom-mode-line t))
                   (writeroom-mode 1)))))))))

(defun ag-mu4e/trash ()
  (interactive)
  (let ((b (current-buffer)))
    (if (eq b (mu4e-get-headers-buffer))
        (mu4e-headers-mark-thread-using-markpair '(delete) t)
      (mu4e-view-mark-thread '(delete)))))

(defun ag-mu4e/find-in-notmuch (msg-id)
  (interactive
   (list (mu4e-message-field (mu4e-message-at-point) :message-id)))
  (notmuch-search (concat "id:" msg-id)))
