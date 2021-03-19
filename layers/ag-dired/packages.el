;;; packages.el --- ag-dired layer packages
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-dired-packages '(
                              ;; dired-rainbow
                              ;; dired-filetype-face
                              (direx :location (recipe :fetcher github
                                                       :repo "agzam/direx-el"))
                              direx-grep
                              treemacs
                              dired-subtree))

(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(defun ag-dired/init-direx ()
  (use-package direx
    :ensure t
    :init
    (require 'direx-project)
    :config
    (defun direx:item-collapse-recursively (item)
      (direx:item-collapse item)
      (dolist (child (direx:item-children item))
        (direx:item-collapse-recursively child)))

    (defun direx:collapse-item-recursively (&optional item)
      (interactive)
      (setq item (or item (direx:item-at-point!)))
      (direx:item-collapse-recursively item)
      (direx:move-to-item-name-part item))

    (defun direx:fit-window ()
      "Shrinks direx window and pushes it to the margin"
      (interactive)
      (when (derived-mode-p 'direx:direx-mode)
        (let ((fit-window-to-buffer-horizontally t))
          (fit-window-to-buffer)
          (window-resize (selected-window) 4 0 nil))))

    (setq direx:file-keymap
          (let ((map (make-sparse-keymap)))
            (define-key map "R" #'direx:do-rename-file)
            (define-key map "C" #'direx:do-copy-files)
            (define-key map "D" #'direx:do-delete-files)
            (define-key map "+" #'direx:create-directory)
            (define-key map "T" #'direx:do-touch)
            (define-key map "j" #'direx:next-item)
            (define-key map "J" #'direx:next-sibling-item)
            (define-key map "k" #'direx:previous-item)
            (define-key map "K" #'direx:previous-sibling-item)
            (define-key map "h" #'direx:collapse-item)
            (define-key map "H" #'direx:collapse-item-recursively)
            (define-key map "l" #'direx:expand-item)
            (define-key map "L" #'direx:expand-item-recursively)
            (define-key map (kbd "RET") #'direx:maybe-find-item)
            (define-key map "a" #'direx:find-item)
            (define-key map "q" #'kill-this-buffer)
            (define-key map "r" #'direx:refresh-whole-tree)
            (define-key map "O" #'direx:find-item-other-window)
            (define-key map "|" #'direx:fit-window)
            (define-key map (kbd "<C-return>") #'direx:set-root)
            (define-key map "^" #'direx:expand-root-to-parent)
            (define-key map "o" #'spacemacs/dired-open-item-other-window-transient-state/body)
            map))

    (spacemacs/set-leader-keys-for-major-mode 'direx:direx-mode
      "o" #'direx:find-item-other-window
      "r" #'direx:refresh-whole-tree
      "\\" #'direx:fit-window
      "RET" #'direx:set-root
      "p" #'direx:expand-root-to-parent)))

(defun ag-dired/post-init-direx ()
  ;; the execution order matters. If not placed in post-init fn it will be
  ;; overwritten by Spacemacs' default keybinging
  (spacemacs/set-leader-keys "pt"  #'direx:jump-to-project-root-or-current-dir))

(defun ag-dired/init-direx-grep ()
  (use-package direx-grep
    :config
    (define-key direx:direx-mode-map (kbd "s") 'direx-grep:grep-item-from-root)
    (define-key direx:direx-mode-map (kbd "S") 'direx-grep:grep-item)
    (define-key direx:direx-mode-map (kbd "`") 'direx-grep:show-all-item-at-point)
    (define-key direx:direx-mode-map (kbd "~") 'direx-grep:show-all-item)
    (spacemacs/set-leader-keys-for-major-mode 'direx:direx-mode
      "gg" #'direx-grep:grep-item-from-root)))

(defun ag-dired/post-init-treemacs ()
  (treemacs-resize-icons 16))

(defun ag-dired/init-dired-subtree ()
  (use-package dired-subtree
    :config
    (evil-define-key 'normal dired-mode-map
      (kbd "TAB") 'dired-subtree-toggle
      "gh" 'dired-subtree-up
      "gl" 'dired-subtree-down
      "gc" 'dired-subtree-cycle
      (kbd "M-j") 'dired-subtree-next-sibling
      (kbd "M-k") 'dired-subtree-previous-sibling)))

;; (defun ag-dired/init-dired-filetype-face ()
;;   (use-package dired-filetype-face
;;     :defer t
;;     :init
;;     (with-eval-after-load 'dired (require 'dired-filetype-face))
;;     :config
;;     (deffiletype-face "js" "yellow")
;;     (deffiletype-face-regexp js :extensions '("js" "json"))
;;     (deffiletype-setup "js")
;;     (setq dired-filetype-xml-regexp (remove "js" dired-filetype-xml-regexp))))

;; (defun ag-dired/init-dired-rainbow ()
;;   (use-package dired-rainbow
;;     :defer t
;;     :init
;;     (with-eval-after-load 'dired
;;       (require 'dired-rainbow))
;;     :config
;;     (dired-rainbow-define js "LightGoldenrod3" ("js"))
;;     (dired-rainbow-define json "salmon3" ("json"))
;;     (dired-rainbow-define dot "gray36" "\\.\\(?:.*$\\)")
;;     (dired-rainbow-define css "DarkSeaGreen4" ("css"))
;;     (dired-rainbow-define html "SpringGreen4" ("html"))))


;;; packages.el ends here
