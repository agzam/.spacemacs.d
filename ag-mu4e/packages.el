(setq ag-mu4e-packages '(mu4e))

(defun mu4e-message-path-matches (msg rx)
  (string-match rx (mu4e-message-field msg :path)))

  ;; (when rx
  ;;   (if (listp rx)
  ;;       ;; if rx is a list, try each one for a match
  ;;       (or (mu4e-message-maildir-matches msg (car rx))
  ;;           (mu4e-message-maildir-matches msg (cdr rx)))
  ;;     ;; not a list, check rx
  ;;     (string-match rx (mu4e-message-field msg :path))))

(defun ag-mu4e/post-init-mu4e ()
  ;; smtpmail
  (require 'smtpmail)
  (require 'starttls)
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        smtpmail-smtp-service 587
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-user "")
  (setq starttls-extra-arguments '("--x509cafile" "/usr/local/etc/openssl/cert.pem"))

  ;; (setq mu4e-account-alist
  ;;       '(("personal"
  ;;          ;; (mu4e-sent-messages-behavior delete)
  ;;          (mu4e-sent-folder "/personal/[Gmail].Sent Mail")
  ;;          (mu4e-drafts-folder "/personal/[Gmail].Drafts")
  ;;          (mu4e-trash-folder "/personal/[Gmail].Trash")
  ;;          (user-mail-address "")
  ;;          (user-full-name "Ag Ibragimov"))

  ;;         ("work"
  ;;          ;; (mu4e-sent-messages-behavior delete)
  ;;          (mu4e-sent-folder "/work/[Gmail].Sent Mail")
  ;;          (mu4e-drafts-folder "/work/[Gmail].Drafts")
  ;;          (mu4e-trash-folder "/work/[Gmail].Trash")
  ;;          (user-mail-address "")
  ;;          (user-full-name "Ag Ibragimov"))))

  ;; (mu4e/mail-account-reset)

  (setq
   ;; mu4e-maildir "~/.mail"
        mu4e-get-mail-command "offlineimap -q"
        mu4e-update-interval 300
        ;; mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-html2text-command "w3m -dump -T text/html"
        mu4e-view-prefer-html t
        mu4e-use-fancy-chars t
        mu4e-view-show-addresses t
        mu4e-enable-notifications t
        mu4e-enable-mode-line t
        mu4e-headers-skip-duplicates t
        mu4e-sent-messages-behavior 'delete
        message-kill-buffer-on-exit t
        mu4e-hide-index-messages t
        mu4e-attachment-dir  "~/Downloads")

  (with-eval-after-load 'mu4e
    (setq
     mu4e-compose-context-policy 'ask
     mu4e-context-policy         'ask
     mu4e-contexts
     `( ,(make-mu4e-context
          :name "home"
          :match-func (lambda (msg)
                        (and msg (mu4e-message-path-matches msg "./mail/personal")))

        ;; :match-func (lambda (msg)
        ;;               (when msg (mu4e-message-contact-field-matches
        ;;                          msg
        ;;                          :to "to.plotnick@gmail.com")))

        :leave-func (lambda () (mu4e-clear-caches))
        :vars '((mu4e-maildir           . "~/.mail/personal")
                (mu4e-get-mail-command  . "offlineimap -a Personal")
                (user-mail-address      . "@gmail.com")
                (user-full-name         . "Ag Ibragimov")
                (mu4e-compose-signature . (concat "Thanks,\n" "Ag"))
                (mu4e-sent-folder       . "/[Gmail].Sent Mail")
                (mu4e-drafts-folder     . "/[Gmail].Drafts")
                (mu4e-trash-folder      . "/[Gmail].Trash")))
     ,(make-mu4e-context
       :name "work"

       :match-func (lambda (msg)
                     (and msg (mu4e-message-path-matches msg "./mail/work")))

     ;; :match-func (lambda (msg)
     ;;               (when msg
     ;;                 (mu4e-message-contact-field-matches
     ;;                  msg
     ;;                  :to "@")))

     :leave-func (lambda () (mu4e-clear-caches))
     :vars '((mu4e-maildir           . "~/.mail/work")
             (mu4e-get-mail-command  . "offlineimap -a Work")
             (user-mail-address      . "")
             (user-full-name         . "Ag Ibragimov" )
             (mu4e-compose-signature . (concat "Thanks,\n" "Ag"))
             (mu4e-sent-folder       . "/[Gmail].Sent Mail")
             (mu4e-drafts-folder     . "/[Gmail].Drafts")
             (mu4e-trash-folder      . "/[Gmail].Bin"))))))

  (with-eval-after-load 'mu4e-alert (mu4e-alert-set-default-style 'notifier))

  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  )

(defun ag-mu4e/pre-init-mu4e ()
  (setq mu4e-installation-path "/usr/local/opt/mu/bin"))
