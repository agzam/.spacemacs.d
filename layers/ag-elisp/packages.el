;; -*- lexical-binding: t -*-

(defconst ag-elisp-packages
  `(helpful
    (let-plist :location local)
    (evilify-edebug :location local)
    eldoc-box))

(defun ag-elisp/init-helpful ()
  (use-package helpful
    :config
    (unbind-key "h" help-map)
    (bind-key "hh" 'helpful-symbol help-map)
    (bind-key "ha" 'helpful-at-point help-map)
    (global-set-key (kbd "C-h k") #'helpful-key)
    (spacemacs/set-leader-keys "hdd" #'helpful-symbol)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
      "hh" 'helpful-at-point
      "ep" 'pp-eval-last-sexp
      "eP" 'eval-print-last-sexp)
    (evil-define-key 'normal helpful-mode-map "q" 'quit-window)

    ;; ensure that browsing in Helpful and Info modes doesn't create additional
    ;; window splits
    (add-to-list
     'display-buffer-alist
     `(,(rx bos (or "*helpful" "*info"))
       (display-buffer-in-direction)
       (direction . right)
       (window . root)
       (window-width . 0.3)))))

(defun ag-elisp/init-let-plist ()
  (use-package let-plist))

(defun ag-elisp/init-evilify-edebug ()
  (use-package evilify-edebug
    :commands (edebug)
    :config
    (evilify-edebug-setup)))

(defun ag-elisp/init-eldoc-box ()
  (use-package eldoc-box
    :config
    (set-face-attribute 'eldoc-box-border nil :background nil)
    (add-hook 'emacs-lisp-mode-hook #'eldoc-box-hover-at-point-mode)))
