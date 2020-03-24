;;; latex-fragments.el --- automatically toggle latex fragments

;;; Commentary:
;; copied from https://ivanaf.com/automatic_latex_fragment_toggling_in_org-mode.html

;;; Code:

(defvar org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")

(defun my/org-latex-fragment--get-current-latex-fragment ()
  "Return the overlay associated with the image under point."
  (car (--select (eq (overlay-get it 'org-overlay-type) 'org-latex-overlay) (overlays-at (point)))))

(defun my/org-in-latex-fragment-p ()
    "Return the point where the latex fragment begins, if inside
  a latex fragment. Else return false"
    (let* ((el (org-element-context))
           (el-type (car el)))
      (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
          (org-element-property :begin el))))

(defun org-latex-fragment-toggle-auto ()
  ;; Wait for the s
  (interactive)
  (while-no-input
    (run-with-idle-timer 0.05 nil 'org-latex-fragment-toggle-helper)))

(defun org-latex-fragment-toggle-helper ()
    "Toggle a latex fragment image "
    (condition-case nil
        (and (eq 'org-mode major-mode)
             (let* ((begin (my/org-in-latex-fragment-p)))
               (cond
                ;; were on a fragment and now on a new fragment
                ((and
                  ;; fragment we were on
                  org-latex-fragment-last
                  ;; and are on a fragment now
                  begin
                  ;; but not on the last one this is a little tricky. as you edit the
                  ;; fragment, it is not equal to the last one. We use the begin
                  ;; property which is less likely to change for the comparison.
                  (not (= begin
                          org-latex-fragment-last)))
                 ;; go back to last one and put image back
                 (save-excursion
                   (goto-char org-latex-fragment-last)
                   (when (my/org-in-latex-fragment-p) (org-latex-preview))
                   ;; now remove current imagea
                   (goto-char begin)
                   (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                     (when ov
                       (delete-overlay ov)))
                   ;; and save new fragment
                   (setq org-latex-fragment-last begin)))

                ;; were on a fragment and now are not on a fragment
                ((and
                  ;; not on a fragment now
                  (not begin)
                  ;; but we were on one
                  org-latex-fragment-last)
                 ;; put image back on
                 (save-excursion
                   (goto-char org-latex-fragment-last)
                   (when (my/org-in-latex-fragment-p)(org-latex-preview)))

                 ;; unset last fragment
                 (setq org-latex-fragment-last nil))

                ;; were not on a fragment, and now are
                ((and
                  ;; we were not one one
                  (not org-latex-fragment-last)
                  ;; but now we are
                  begin)
                 (save-excursion
                   (goto-char begin)
                   ;; remove image
                   (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                     (when ov
                       (delete-overlay ov)))
                   (setq org-latex-fragment-last begin)))
                ;; else not on a fragment
                ((not begin)
                 (setq org-latex-fragment-last nil)))))
      (error nil)))



;; (add-hook 'post-command-hook 'org-latex-fragment-toggle-auto)
(setq org-latex-fragment-toggle-helper (byte-compile 'org-latex-fragment-toggle-helper))
(setq org-latex-fragment-toggle-auto (byte-compile 'org-latex-fragment-toggle-auto))

(provide 'latex-fragments)

;;; latex-fragments.el ends here
