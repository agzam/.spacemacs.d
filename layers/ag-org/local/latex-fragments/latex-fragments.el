;;; latex-fragments.el --- automatically toggle latex fragments

;;; Commentary:
;; from http://slumpy.org/blog/2017-02-01-automatic-latex-preview-in-org-mode/

;;; Code:

(defvar kk/org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")

(defun kk/org-in-latex-fragment-p ()
  "Return the point where the latex fragment begins, if inside
a latex fragment. Else return false"
  (let* ((el (org-element-context))
         (el-type (car el)))
    (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
         (org-element-property :begin el))))

(defun kk/org-latex-fragment-toggle ()
  "Toggle a latex fragment image"
  (and (eq 'org-mode major-mode)
       (let ((begin (kk/org-in-latex-fragment-p)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            kk/org-latex-fragment-last
            ;; and are on a fragment now
            begin

            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (and kk/org-latex-fragment-last
                      (= begin
                         kk/org-latex-fragment-last))))
           ;; go back to last one and put image back, provided there is still a fragment there
           (save-excursion
             (goto-char kk/org-latex-fragment-last)
             (when (kk/org-in-latex-fragment-p) (org-preview-latex-fragment))

             ;; now remove current image
             (goto-char begin)
             (let ((ov (loop for ov in (org--list-latex-overlays)
                             if
                             (and
                              (<= (overlay-start ov) (point))
                              (>= (overlay-end ov) (point)))
                             return ov)))
               (when ov
                 (delete-overlay ov)))
             ;; and save new fragment
             (setq kk/org-latex-fragment-last begin)))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not begin)
            ;; but we were on one
            kk/org-latex-fragment-last)
           ;; put image back on, provided that there is still a fragment here.
           (save-excursion
             (goto-char kk/org-latex-fragment-last)
             (when (kk/org-in-latex-fragment-p) (org-preview-latex-fragment)))

           ;; unset last fragment
           (setq kk/org-latex-fragment-last nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not kk/org-latex-fragment-last)
            ;; but now we are
            begin)
           ;; remove image
           (save-excursion
             (goto-char begin)
             (let ((ov (loop for ov in (org--list-latex-overlays)
                             if
                             (and
                              (<= (overlay-start ov) (point))
                              (>= (overlay-end ov) (point)))
                             return ov)))
               (when ov
                 (delete-overlay ov))))
           (setq kk/org-latex-fragment-last begin))))))

(provide 'latex-fragments)

;;; latex-fragments.el ends here
