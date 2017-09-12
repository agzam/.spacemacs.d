(defun ag/region-or-word-at-point-str ()
  "Returns string of selected region or word at point"
  (let* ((bds (if (use-region-p)
                  (cons (region-beginning) (region-end))
                (bounds-of-thing-at-point 'word)))
         (p1 (car bds))
         (p2 (cdr bds)))
    (buffer-substring-no-properties p1 p2)))

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
 [_+_/_=_/_j_] in        [_p_] prev. display [_f_]^^^ fullscreen
 [_-_/_k_]^^   out       [_n_] next display  [_m_]^^^ max-frame
 [_0_]^^^^   reset                         [_q_]^^^ quit
"

  :bindings
  ("+" zoom-frm-in)
  ("=" zoom-frm-in)
  ("j" zoom-frm-in)
  ("-" zoom-frm-out)
  ("k" zoom-frm-out)
  ("0" zoom-frm-unzoom)
  ("f" (spacemacs/toggle-frame-fullscreen-non-native))
  ("m" (spacemacs/toggle-maximize-frame))
  ("n" (ag/move-frame-one-display "North"))
  ("p" (ag/move-frame-one-display "South"))
  ("q" nil :exit t))

;; remove visual marks overlay after marks are deleted
(advice-add 'evil-delete-marks :after
            (lambda (&rest args)
              (evil-visual-mark-render)))
