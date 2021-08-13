(defconst ag-cal-packages '(calfw
                            ;; calfw-gcal
                            ))

(defun ag-cal/init-calfw()
  (use-package calfw))

(defun ag-cal/init-calfw-gcal ()
  (use-package calfw-gcal))

