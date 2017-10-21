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
  "shows Hammerspoon's hs.alert popup with a MESSAGE"
  (when (and message (eq system-type 'darwin))
    (call-process (executable-find "hs")
                  nil 0 nil
                  "-c"
                  (concat "hs.alert.show(\"" message "\", 1)"))))

(defun ag/atomic-edit-start ()
  (remove-hook 'markdown-mode-hook 'spacemacs/activate-mmm-mode)
  (remove-hook 'markdown-mode-hook 'spacemacs//cleanup-org-tables-on-save))

(defun ag/atomic-edit-done ()
  (kill-new (buffer-string) t)
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

(defun ag/switch-to-app (pid)
  "Using third party tools tries to switch to the app with the given PID"
  (when (and pid (eq system-type 'darwin))
    (call-process (executable-find "hs") nil 0 nil "-c"
                  (concat "require(\"emacs\").switchToApp (\"" pid "\")"))))

(defun -slide-item (l e dir)
  "Slides item E in list L to the right or left position
DIR - is positive or negative number (relative to the position of E). Item can't exceed boundaries of the list - e.g. if given DIR value larger than length of the list - item becomes the last.
"
  (let* ((c-pos (-elem-index e l)))
    (if c-pos
        (let* ((f (cond ((<= (- (length l) 1) dir) (- (length l) 1))
                        (t dir)))
               (new-p (+ c-pos f)))
          (-insert-at new-p e (-remove-at c-pos l))) l)))

(defun persp-slide (dir)
  (setq persp-names-cache (-slide-item persp-names-cache (persp-name (get-current-persp)) dir))
  (spacemacs/layouts-transient-state/body))

(defun persp-slide-to-left ()
  (interactive)
  (persp-slide -1))

(defun persp-slide-to-right ()
  (interactive)
  (persp-slide 1))

(defun persp-add-keybindings ()
  (define-key spacemacs/layouts-transient-state/keymap (kbd "<") #'persp-slide-to-left)
  (define-key spacemacs/layouts-transient-state/keymap (kbd ">") #'persp-slide-to-right))

(add-hook 'persp-mode-hook #'persp-add-keybindings)
