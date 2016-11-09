(defun endless/4clojure-check-and-proceed ()
  "Check the answer and show the next question if it worked."
  (interactive)
  (unless
      (save-excursion
        ;; Find last sexp (the answer).
        (goto-char (point-max))
        (forward-sexp -1)
        ;; Check the answer.
        (cl-letf ((answer
                   (buffer-substring (point) (point-max)))
                  ;; Preserve buffer contents, in case you failed.
                  ((buffer-string)))
          (goto-char (point-min))
          (while (search-forward "__" nil t)
            (replace-match answer))
          (string-match "failed." (4clojure-check-answers))))
    (4clojure-next-question)))


(defun endless/4clojure-login (user pwd)
  "Login to 4clojure"
  (interactive "sWhat's your name? \nsAnd your password ")
  (request
   "http://www.4clojure.com/login"
   :type "POST"
   :sync t
   :headers '(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:28.0) Gecko/20100101  Firefox/28.0")
              ("Referer" . "http://www.4clojure.com/login"))
                                        ;   :parser 'buffer-string
   :data `(("user" . ,user) ("pwd" . ,pwd))
   :success (function*
             (lambda (&key data &allow-other-keys) data))
  ; when server send 302 header, `request` redirect request with original method POST,
  ; So 4clojure will not handle this redirect and given 404
   :status-code '((404 . (lambda (&rest _) (message "login successful!"))))))
