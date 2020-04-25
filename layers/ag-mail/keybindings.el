;;; keybindings.el --- ag-mail layer keybindings
;;
;; Copyright (c) 2017 Ag Ibragimov
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Code:

(defun ag-mail/set-mu4e-keys ()
  (dolist (m '(mu4e-headers-mode-map mu4e-view-mode-map))
    (define-key m (kbd "C-h") nil)
    (define-key m (kbd "C-=") #'mu4e-headers-split-view-grow)
    (define-key m (kbd "C--") #'mu4e-headers-split-view-shrink)
    (define-key m (kbd "D") #'mu4e-headers-mark-for-trash)
    (define-key m (kbd "M-d") #'mu4e-headers-mark-for-delete)
    (define-key m (kbd "C-k") #'mu4e-view-headers-prev)
    (define-key m (kbd "C-j") #'mu4e-view-headers-next)
    (evil-define-key 'normal m (kbd "C-j") #'mu4e-view-headers-next)
    (evilified-state-evilify-map m
      :mode mu4e-headers-mode
      :bindings
      (kbd "J") (lambda ()
                  (interactive)
                  (mu4e-view-mark-thread '(unmark)))
      (kbd "C-j") #'mu4e-view-headers-next
      (kbd "C-k" ) #'mu4e-view-headers-prev
      (kbd "C-h") nil)
    (evilified-state-evilify-map m
      :mode mu4e-view-mode
      :bindings
      (kbd "C-j") #'mu4e-view-headers-next
      (kbd "C-k" ) #'mu4e-view-headers-prev))

  (define-key mu4e-headers-mode-map (kbd "M-SPC") #'hydra-mu4e-headers/body)
  (define-key mu4e-headers-mode-map (kbd "TAB") 'mu4e-headers-toggle-thread-folding)

  (evil-define-key 'evilified mu4e-main-mode-map "j" 'evil-next-visual-line)

  ;; prevent accidental closing of headers when you just need to collapse view buffer
  (define-key mu4e-headers-mode-map "q" nil)
  (define-key mu4e-headers-mode-map (kbd "C-q") #'mu4e~headers-quit-buffer)

  (define-key mu4e-view-mode-map (kbd "d")
    (lambda () (interactive) (mu4e-view-mark-subthread '(trash))))

  (define-key mu4e-headers-mode-map (kbd "d")
    (lambda ()
      (interactive)
      (when (mu4e-headers-thread-folded?)
        (mu4e-headers-toggle-thread-folding))
      (mu4e-headers-mark-thread-using-markpair '(trash) t)))

  (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
    dotspacemacs-major-mode-leader-key 'message-send-and-exit
    "c" 'message-send-and-exit
    "k" 'mu4-message-kill-buffer
    "d" 'message-dont-send         ; saves as draft
    "a" 'mml-attach-file
    "o" 'org-mu4e-compose-org-mode))


;;; keybindings.el ends here
