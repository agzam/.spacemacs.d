(setq ag-4clojure-packages '(4clojure))

(setq ag-4clojure-excluded-packages '())

(defun ag-4clojure/init-4clojure ()
  (use-package 4clojure
    :defer t
    :init

    (defadvice 4clojure-open-question (around 4clojure-open-question-around)
      "Start a cider/nREPL connection if one hasn't already been started when
opening 4clojure questions"
      ad-do-it
      (unless cider-current-clojure-buffer
        (cider-jack-in)))

    (defadvice 4clojure/start-new-problem
        (after endless/4clojure/start-new-problem-advice () activate)
      ;; Prettify the 4clojure buffer.
      (goto-char (point-min))
      (forward-line 2)
      (forward-char 3)
      (fill-paragraph)
      ;; Position point for the answer
      (goto-char (point-max))
      (insert "\n\n\n")
      (forward-char -1)
      ;; Define our key.
      (local-set-key (kbd "s-n") #'endless/4clojure-check-and-proceed))))
