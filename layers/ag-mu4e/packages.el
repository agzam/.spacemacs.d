;;; packages.el --- ag-mu4e layer packages
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
(defconst ag-mu4e-packages '((mu4e :location site)
                             (mu4e-thread-folding
                              :location
                              (recipe :fetcher github
                                      :repo "rougier/mu4e-thread-folding"))))

(defun ag-mu4e/post-init-mu4e ()
  (with-eval-after-load 'mu4e
    (setq mu4e-context-policy 'pick-first
          mu4e-compose-context-policy 'pick-first
          mu4e-contexts
          `(,(make-mu4e-context
              :name "home"
              :enter-func (lambda () (mu4e-message "Switch to agzam.ibragimov@gmail.com"))
              ;; leave-func not defined
              :match-func (lambda (msg) (when msg (mu4e-message-maildir-matches msg "^/home/")))
              :vars '((smtpmail-smtp-server . "smtp.gmail.com")
                      (mu4e-sent-folder . "/home/[Gmail]/Sent Mail")
                      (mu4e-trash-folder . "/home/[Gmail]/Trash")
                      (mu4e-refile-folder . "/home/[Gmail]/All Mail")
                      (mu4e-drafts-folder . "/home/[Gmail]/Drafts")
                      (user-mail-address . "agzam.ibragimov@gmail.com")
                      (user-full-name . "Ag Ibragimov")
                      (smtpmail-smtp-user . "agzam.ibragimov@gmail.com")
                      (smtpmail-mail-address . "agzam.ibragimov@gmail.com")
                      (message-signature-file . "~/.mail/home/.signature")
                      (mu4e-compose-signature . (concat "Thanks,\n" "Ag\n"))))))

    (add-to-list
     'mu4e-header-info-custom
     '(:empty . (:name "Empty" :shortname "" :function (lambda (msg) "  "))))

    (setq mu4e-headers-fields
          '((:empty . 4)
            (:flags . 6)
            (:human-date . 10)
            (:mailing-list . 15)
            (:from . 22)
            (:subject . nil)))

    (setq mu4e-use-fancy-chars t
          mu4e-headers-draft-mark '("D" . "🕗")
          mu4e-headers-flagged-mark '("F" . "⚑")
          mu4e-headers-new-mark '("N" . "•")
          mu4e-headers-passed-mark '("P" . "❯")
          mu4e-headers-replied-mark '("R" . "✏")
          mu4e-headers-seen-mark '("S" . "✓")
          mu4e-headers-trashed-mark '("T" . "☠")
          mu4e-headers-attach-mark '("a" . "📎")
          mu4e-headers-encrypted-mark '("x" . "🔐")
          mu4e-headers-signed-mark '("s" . "🎵")
          mu4e-headers-unread-mark '("u". "●"))
    )

  (set-face-attribute 'mu4e-header-highlight-face nil :weight 'normal)
  (set-face-attribute 'mu4e-unread-face nil :weight 'normal)

  (add-hook 'mu4e-main-mode-hook 'ag-mu4e/set-mu4e-keys)
  (add-hook 'mu4e-headers-mode-hook 'ag-mu4e/set-mu4e-keys)
  (add-hook 'mu4e-view-mode-hook 'ag-mu4e/set-mu4e-keys)

  (add-hook 'mu4e-view-mode-hook 'ag-mu4e/prepare-view)
  )

(defun ag-mu4e/init-mu4e-thread-folding ()
  (use-package mu4e-thread-folding
    :commands (mu4e-main-mode)
    :config
    (mu4e-thread-folding-load)))


;;; packages.el ends here
