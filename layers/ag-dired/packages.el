(setq ag-dired-packages '(
                          ;; dired-rainbow
                          ;; dired-filetype-face
                          direx))

;; List of packages to exclude.
(setq ag-dired-excluded-packages '())

(defun ag-dired/init-direx ()
  (use-package direx
    :defer t
    :init
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
      (interactive)
      (when (derived-mode-p 'direx:direx-mode) 
        (let ((fit-window-to-buffer-horizontally t))
          (fit-window-to-buffer)
          (window-resize (selected-window) 4 0 nil))))

    (defvar direx:file-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map "R" 'direx:do-rename-file)
        (define-key map "C" 'direx:do-copy-files)
        (define-key map "D" 'direx:do-delete-files)
        (define-key map "+" 'direx:create-directory)
        (define-key map "T" 'direx:do-touch)
        (define-key map "j" 'direx:next-item)
        (define-key map "J" 'direx:next-sibling-item)
        (define-key map "k" 'direx:previous-item)
        (define-key map "K" 'direx:previous-sibling-item)
        (define-key map "h" 'direx:collapse-item)
        (define-key map "H" 'direx:collapse-item-recursively)
        (define-key map "l" 'direx:expand-item)
        (define-key map "L" 'direx:expand-item-recursively)
        (define-key map (kbd "RET") 'direx:maybe-find-item)
        (define-key map "a" 'direx:find-item)
        (define-key map "q" 'kill-this-buffer)
        (define-key map "r" 'direx:refresh-whole-tree)
        (define-key map "O" 'direx:find-item-other-window)
        (define-key map "|" 'direx:fit-window)
        (define-key map "o" 'spacemacs/dired-open-item-other-window-transient-state/body)
        map))))

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
