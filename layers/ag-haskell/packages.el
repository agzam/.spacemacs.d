;;; packages.el --- ag-haskell layer packages.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-haskell-packages '(company-ghci))

(defun ag-haskell/post-init-company-ghci ()
  (use-package company-ghci
    :ensure t
    :config
    (push 'company-ghci company-backends)
    (add-hook 'haskell-mode-hook 'company-mode)
    (add-hook 'haskell-interactive-mode-hook 'company-mode)))

(setq haskell-process-type 'stack-ghci
      haskell-process-log t)

(with-eval-after-load 'persp-mode
  (spacemacs|define-custom-layout "@haskell-book"
    :binding "h"
    :body (progn
            (spacemacs/window-split-double-columns t)
            (call-interactively #'haskell-interactive-bring)
            (spacemacs//open-in-external-app "/home/ag/SyncMobile/Books/Haskell/Haskell_Programming_(EARLY_ACCESS)/haskell-programming-1.0RC2-ereader.pdf")
            (run-at-time "0.5 sec" nil (lambda ()
                                         (set-window-buffer (nth 0 (window-list-sorted)) "*haskell*")
                                         (select-window (frame-first-window))
                                         (split-window-vertically)
                                         (find-file "~/Sandbox/haskell-book-exercises")
                                         )))))

;; ;;; packages.el ends here
