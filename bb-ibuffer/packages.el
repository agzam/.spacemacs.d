(setq bb-ibuffer-packages '(ibuffer))

(defun bb-ibuffer/post-init-ibuffer ()
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 50 30 :left :elide) ; change: 30s were originally 18s
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename)))
  )

(defun bb-ibuffer/pre-init-ibuffer ()
  (with-eval-after-load 'ibuffer
    (require 'projectile)
    (setq ibuffer-saved-filter-groups
          (list (cons "Default"
                      (append
                       (mapcar (lambda (it)
                                 (let ((name (file-name-nondirectory
                                              (directory-file-name it))))
                                   `(,name (filename . ,(expand-file-name it)))))
                               projectile-known-projects)
                       `(("Org" (mode . org-mode))
                         ("Dired" (mode . dired-mode))
                         ("IRC" (mode . erc-mode))
                         ("Emacs" (or (name . "\\*Messages\\*")
                                      (name . "\\*Compile-Log\\*")
                                      (name . "\\*scratch\\*")
                                      (name . "\\*spacemacs\\*")
                                      (name . "\\*emacs\\*")))
                         ("Magit" (name . "\\*magit"))
                         ("Help" (name . "\\*Help\\*"))
                         ("Helm" (name . "\\*helm"))
                         )))))
    (add-hook 'ibuffer-mode-hook
              (defun bb-ibuffer/switch-ibuffer-group ()
                (ibuffer-switch-to-saved-filter-groups "Default")))
    (add-hook 'ibuffer-mode-hook 'ibuffer-auto-mode)))
