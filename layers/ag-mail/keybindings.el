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

(with-eval-after-load 'mu4e
  (dolist (m '(mu4e-headers-mode-map mu4e-view-mode-map))
    (evil-define-key 'evilified m (kbd "C-h") nil)
    (define-key m (kbd "C-h") nil)
    (define-key m (kbd "C-=") 'mu4e-headers-split-view-grow)
    (define-key m (kbd "C--") 'mu4e-headers-split-view-shrink))

  (define-key mu4e-headers-mode-map (kbd "M-SPC") #'hydra-mu4e-headers/body)

  ;; prevent accidental closing of headers when you just need to collapse view buffer
  (define-key mu4e-headers-mode-map "q" nil)
  (define-key mu4e-headers-mode-map (kbd "C-q") #'mu4e~headers-quit-buffer)

  (evil-define-key 'evilified mu4e-main-mode-map "j" 'evil-next-visual-line)

  (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
    dotspacemacs-major-mode-leader-key 'message-send-and-exit
    "c" 'message-send-and-exit
    "k" 'mu4-message-kill-buffer
    "d" 'message-dont-send         ; saves as draft
    "a" 'mml-attach-file
    "o" 'org-mu4e-compose-org-mode))

;;; keybindings.el ends here
