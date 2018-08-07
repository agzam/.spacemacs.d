;;; packages.el --- ag-mail layer packages
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
(defconst ag-mail-packages '())

(with-eval-after-load 'mu4e
  (setq mu4e-maildir "~/.mail"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 600
        mu4e-compose-signature-auto-include t
        mu4e-view-show-images t
        mu4e-view-show-addresses t

        ;; rename files when moving, needed for mbsync
        mu4e-change-filenames-when-moving t

        mu4e-compose-dont-reply-to-self t
        mu4e-compose-format-flowed nil
        fill-flowed-encode-column 280
        mu4e-user-mail-address-list '("agzam.ibragimov@gmail.com" "to.plotnick@gmail.com")
        mu4e-compose-complete-only-personal t
        mu4e-enable-async-operations t
        org-mu4e-link-query-in-headers-mode nil
        org-mu4e-convert-to-html t

        ;; customize the reply-quote-string
        message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n"
        ;; choose to use the formatted string
        message-citation-line-function 'message-insert-formatted-citation-line)

  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-queue-mail nil
        smtpmail-queue-dir "~/.mail/queue/cur"
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        mu4e-sent-messages-behavior 'delete
        mail-user-agent 'mu4e-user-agent)

  (setq
   mu4e-contexts
   `( ,(make-mu4e-context
        :name "home"
        :enter-func (lambda ()
                      (mu4e-message "Switch to agzam.ibragimov@gmail.com"))
        ;; leave-func not defined
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg :to "agzam.ibragimov@gmail.com")))
        :vars '((user-mail-address . "agzam.ibragimov@gmail.com")
                (user-full-name . "Ag Ibragimov")
                (mu4e-compose-signature . (concat "Thanks,\n" "Ag\n")))))
   mu4e-compose-context-policy 'ask-if-none)

  (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)
  (add-hook 'mu4e-compose-mode-hook #'spacemacs/toggle-visual-line-navigation-on)

  (spacemacs/set-leader-keys-for-major-mode 'mu4e-compose-mode
    dotspacemacs-major-mode-leader-key 'message-send-and-exit
    "c" 'message-send-and-exit
    "k" 'mu4-message-kill-buffer
    "d" 'message-dont-send         ; saves as draft
    "a" 'mml-attach-file
    "o" 'org-mu4e-compose-org-mode)

  (setq mu4e-view-actions
        '(("capture message" . mu4e-action-capture-message)
          ("view in browser" . mu4e-action-view-in-browser)
          ("show this thread" . mu4e-action-show-thread)
          ("View as pdf" . mu4e-action-view-as-pdf)))
  )

;;; packages.el ends here
