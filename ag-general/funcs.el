(defun get-default-height ()
  (+ 8 (/ (- (display-pixel-height) 120)
          (frame-char-height))))

(defun get-default-width ()
  (+ 13 (/ (- (display-pixel-width) 120)
           (frame-char-width))))

(defun max-frame () 
  "maximizing frame without borders"
  (interactive)
  (set-frame-position nil 0 -21)
  (set-frame-height (selected-frame) (get-default-height))
  (set-frame-width (selected-frame) (get-default-width)))

(defun ag/region-or-symbol-bounds ()
  (if (region-active-p)
      (cons (region-beginning)
            (region-end))
    (bounds-of-thing-at-point 'symbol)))

(defun ag/buffer-substring ()
  "grabs selected region's text, if any"
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (word-at-point)))

(defun ag/helm-google-suggest ()
  "Smarter helm-google-suggest - searches for selected text (if any)"
  (interactive)
  (let ((sub-str (ag/buffer-substring)))
    (if sub-str
        (helm-google-suggest-action sub-str)
      (helm-google-suggest))))

(defun notify-osx (title message)
  (call-process (executable-find "terminal-notifier")
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" message))

(defun hs-alert (message)
  (when message
    (call-process (executable-find "hs")
                  nil 0 nil
                  (concat "-c" "hs.alert.show(\"" message "\", 1)"))))

(defun ag/switch-focus-to-emacs-frame ()
  (shell-command "open -a \"Emacs\""))

(defun ag/switch-focus-to-chrome ()
  (shell-command "open -a \"Google Chrome\""))


