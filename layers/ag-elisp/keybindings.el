;; -*- lexical-binding: t -*-

;;;; don't quit helpful on Esc
(evil-define-key 'motion help-mode-map (kbd "<escape>") nil)

;; have to wrap it, so it overrides the default bindings
(spacemacs/defer-until-after-user-config
 (lambda ()
   (spacemacs|spacebind
    "Files manipulation."
    :global
    (("f" "Files"
      ("e" "Emacs/Spacemacs"
       ("i" ag/find-user-init-file "Open Emacs \"init.el\"")))))))

(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "gr" #'xref-find-references
  "gd" #'xref-find-definitions
  "gD" #'xref-find-definitions-other-window
  "," #'elisp-slime-nav-find-elisp-thing-at-point)
