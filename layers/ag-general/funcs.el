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
  (when (eq system-type 'darwin)
    (call-process (executable-find "terminal-notifier")
                  nil 0 nil
                  "-group" "Emacs"
                  "-title" title
                  "-sender" "org.gnu.Emacs"
                  "-message" message)))

(defun hs-alert (message)
  (when (and message (eq system-type 'darwin)) 
    (call-process (executable-find "hs")
                  nil 0 nil
                  (concat "-c" "hs.alert.show(\"" message "\", 1)"))))

(defun ag/atomic-edit-start ()
  (remove-hook 'markdown-mode-hook 'spacemacs/activate-mmm-mode)
  (remove-hook 'markdown-mode-hook 'spacemacs//cleanup-org-tables-on-save))

(defun ag/atomic-edit-done ()
  (when (eq system-type 'darwin)
    (shell-command "open -a \"Google Chrome\"")))

(defun ag/fix-frame ()
  "Toggle fullscreen off and on. OS X workaround."
  (when (spacemacs/toggle-fullscreen-frame-p)
      (progn
        (spacemacs/toggle-fullscreen-frame-off)
        (spacemacs/toggle-fullscreen-frame-on))))

(defun ag/move-frame-one-display (direction)
  (when (eq system-type 'darwin)
    (let* ((hs (executable-find "hs"))
           (cmd (concat "hs.window.focusedWindow():moveOneScreen" direction "()")))
      (call-process hs
                    nil 0 nil
                    (concat "-c" cmd))
      (ag/fix-frame))))

(spacemacs|define-transient-state zoom-frm
  :title "Zoom Frame Transient State"
  :doc "
 Zoom^^^^^^              Move^^^^^              Other^^
 ────^^^^^^───────────── ────^^^^^^───────────── ─────^^──────────
 [_+_/_=_/_j_] in        [_p_] prev. display [_m_]^^^ fullscreen
 [_-_/_k_]^^   out       [_n_] next display  [_q_]^^^ quit
 [_0_]^^^^     reset     "

  :bindings
  ("+" zoom-frm-in)
  ("=" zoom-frm-in)
  ("j" zoom-frm-in)
  ("-" zoom-frm-out)
  ("k" zoom-frm-out)
  ("0" zoom-frm-unzoom)
  ("m" (spacemacs/toggle-frame-fullscreen-non-native))
  ("n" (ag/move-frame-one-display "North"))
  ("p" (ag/move-frame-one-display "South"))
  ("q" nil :exit t))

;; remove visual marks overlay after marks are deleted
(advice-add 'evil-delete-marks :after
            (lambda (&rest args)
              (evil-visual-mark-render)))
