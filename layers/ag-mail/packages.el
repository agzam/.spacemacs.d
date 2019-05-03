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
(defconst ag-mail-packages '(persp-mode))

(defun ag-mail/post-init-persp-mode ()
  (spacemacs|define-custom-layout mu4e-spacemacs-layout-name
    :binding mu4e-spacemacs-layout-binding
    :body
    (progn
      (call-interactively 'mu4e)
      (call-interactively 'mu4e-update-index)

      (define-advice mu4e~stop (:after nil kill-mu4e-layout-after-mu4e~stop)
        (when mu4e-spacemacs-kill-layout-on-exit
          (persp-kill mu4e-spacemacs-layout-name)))))
  ;; TODO: remove when fix available for Spacemacs
  ;; Temporarily disables mu4e layer future because it very often screws up window configuration
  (dolist (h (mapcar #'derived-mode-hook-name
                     '(mu4e-main-mode
                       mu4e-headers-mode
                       mu4e-view-mode
                       mu4e-compose-mode)))
    (remove-hook h #'spacemacs-layouts/add-mu4e-buffer-to-persp)))

(with-eval-after-load 'mu4e-alert
  ;; Enable Desktop notifications
  (mu4e-alert-set-default-style 'notifications))

(with-eval-after-load 'mu4e
  (setq mu4e-maildir "~/.mail"
        mu4e-get-mail-command "mbsync --all --new --delete --flags --renew --pull --push --create --expunge --verbose"
        mu4e-update-interval 600
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-enable-notifications nil
        mu4e-enable-mode-line t
        mu4e-headers-skip-duplicates t
        ;; rename files when moving, needed for mbsync
        mu4e-index-update-in-background t
        mu4e-change-filenames-when-moving t
        mu4e-headers-has-child-prefix '("."  . "◼ ")
        mu4e-headers-default-prefix '(" "  . "│ ")
        mu4e-compose-dont-reply-to-self t
        mu4e-compose-format-flowed t
        fill-flowed-encode-column 5000
        mu4e-user-mail-address-list '("agzam.ibragimov@gmail.com" "to.plotnick@gmail.com" "ag@mayvenn.com")
        mu4e-compose-complete-only-personal t
        mu4e-enable-async-operations nil
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
        mu4e-sent-messages-behavior 'sent
        mail-envelope-from 'header
        mail-user-agent 'mu4e-user-agent)

  (defun mu4e-message-maildir-matches (msg rx)
    (string-match rx (mu4e-message-field msg :maildir)))

  (setq
   mu4e-context-policy 'pick-first
   mu4e-compose-context-policy 'ask
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
               (smtpmail-smtp-user . "to.plotnick@gmail.com")
               (message-signature-file . "~/.mail/home/.signature")
               (mu4e-compose-signature . (concat "Thanks,\n" "Ag\n"))))
     ,(make-mu4e-context
       :name "work"
       :enter-func (lambda () (mu4e-message "Switch to work"))
       ;; leave-func not defined
       :match-func (lambda (msg) (when msg (mu4e-message-maildir-matches msg "^/work/")))
       :vars '((smtpmail-smtp-server . "smtp.office365.com")
               (mu4e-sent-folder . "/work/Sent Items")
               (mu4e-trash-folder . "/work/Deleted Items")
               (mu4e-refile-folder . "/work/Archive")
               (mu4e-drafts-folder . "/work/Drafts")
               (user-mail-address . "ag@dividendfinance.com")
               (user-full-name . "Ag Ibragimov")
               (smtpmail-smtp-user . "ag.ibragimov@dividendfinance.com")
               (message-signature-file . "~/.mail/work/.signature")
               (mu4e-compose-signature . (concat "-----\nAg Ibragimov\nSoftware Developer | Dividend Finance\ndividendnow.com"))))))

  (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)
  (add-hook 'mu4e-compose-mode-hook #'spacemacs/toggle-visual-line-navigation-on)

  (setq mu4e-view-actions
        '(("capture message" . mu4e-action-capture-message)
          ("view in browser" . mu4e-action-view-in-browser)
          ("show this thread" . mu4e-action-show-thread)
          ("View as pdf" . mu4e-action-view-as-pdf)))

  (add-to-list
   'mu4e-header-info-custom
   '(:foldername . (:name "Folder information"
                          :shortname "Folder"
                          :help "Message short storage information"
                          :function (lambda (msg)
                                      (let ((shortaccount)
                                            (maildir (or (mu4e-message-field msg :maildir) ""))
                                            (mailinglist (or (mu4e-message-field msg :mailing-list) "")))
                                        (if (not (string= mailinglist ""))
                                            (setq mailinglist (mu4e-get-mailing-list-shortname mailinglist)))
                                        (when (not (string= maildir ""))
                                          (setq shortaccount
                                                (substring
                                                 (replace-regexp-in-string "^/\\(\\w+\\)/.*$" "\\1" maildir)
                                                 0 1))
                                          (setq maildir (replace-regexp-in-string ".*/\\([^/]+\\)$" "\\1" maildir))
                                          (if (> (length maildir) 15)
                                              (setq maildir (concat (substring maildir 0 14) "…")))
                                          (setq maildir (concat "[" shortaccount "] " maildir)))
                                        (cond
                                         ((and (string= maildir "")
                                               (not (string= mailinglist "")))
                                          mailinglist)
                                         ((and (not (string= maildir ""))
                                               (string= mailinglist ""))
                                          maildir)
                                         ((and (not (string= maildir ""))
                                               (not (string= mailinglist "")))
                                          (concat maildir " (" mailinglist ")"))
                                         (t "")))))))

  (defun ed/get-mail-header (header-name path)
    (replace-regexp-in-string
     "[ \t\n]*$"
     ""
     (shell-command-to-string
      (concat "/usr/bin/sed -n '/^" header-name ":/I{:loop t;h;n;/^ /{H;x;s/\\n//;t loop};x;p}' '" path "' | sed -n 's/^" header-name ": \\(.*\\)$/\\1/Ip'"))))

  (defun ed/get-origin-mail-system-header (msg)
    (let ((path (or (mu4e-message-field msg :path) "")))
      (if (or (string= path "")
              (not (file-readable-p path)))
          "no path found"
        (let ((xmailer (ed/get-mail-header "x-mailer" path))
              (useragent (ed/get-mail-header "user-agent" path)))
          (if (string= xmailer useragent)
              xmailer
            (cond
             ((string= xmailer "") useragent)
             ((string= useragent "") xmailer)
             (t (concat xmailer " (xmailer)\n" useragent " (user-agent)"))))))))

  (add-to-list 'mu4e-header-info-custom
               '(:useragent . (:name "User-Agent"
                                     :shortname "UserAgt."
                                     :help "Mail client used by correspondant"
                                     :function ed/get-origin-mail-system-header)))

  (setq mu4e-headers-fields
        '((:flags . 5)
          (:human-date . 22)
          (:size . 6)
          (:foldername . 25)
          (:from-or-to . 25)
          (:subject . nil))

        mu4e-headers-date-format "%a %d %b %Y %H:%M"
        mu4e-headers-time-format "%H:%M"
        mu4e-use-fancy-chars nil
        mu4e-view-fields '(:from :to :cc :subject :flags :date :maildir :mailing-list :tags :useragent :attachments :signature :decryption))

  (add-hook 'mu4e-view-mode-hook #'spacemacs/toggle-visual-line-navigation-on)

  (mu4e-maildirs-extension)

  )

;;; packages.el ends here
