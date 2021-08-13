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
(defconst ag-mail-packages '(
                             ;; persp-mode
                             ;; mu4e
                             ;; (mu4e-thread-folding
                             ;;  :location
                             ;;  (recipe :fetcher github
                             ;;          :repo "rougier/mu4e-thread-folding"))
                             ))

;; things I do for love... I mean, gccemacs
;; (with-eval-after-load 'use-package
;;   (use-package mu4e
;;     :load-path "/usr/local/Cellar/mu/1.4.13/share/emacs/site-lisp/mu/mu4e"))

(defun ag-mail/post-init-persp-mode ()
;;   (spacemacs|define-custom-layout mu4e-spacemacs-layout-name
;;     :binding mu4e-spacemacs-layout-binding
;;     :body
;;     (progn
;;       (call-interactively 'mu4e)
;;       (call-interactively 'mu4e-update-index)

;;       (define-advice mu4e~stop (:after nil kill-mu4e-layout-after-mu4e~stop)
;;         (when mu4e-spacemacs-kill-layout-on-exit
;;           (persp-kill mu4e-spacemacs-layout-name)))

;;       (define-advice mu4e-headers-quit-buffer
;;           (:after nil mu4e-update-mail-after-mu4e-headers-quit-buffer)
;;         (mu4e-update-mail-and-index :run-in-the-background))))

  ;; TODO: remove when fix available for Spacemacs
  ;; Temporarily disables mu4e layer future because it very often screws up window configuration
  (dolist (h (mapcar #'derived-mode-hook-name
                     '(mu4e-main-mode
                       mu4e-headers-mode
                       mu4e-view-mode
                       mu4e-compose-mode)))
    (remove-hook h #'spacemacs-layouts/add-mu4e-buffer-to-persp)))

(defun ag-mail/post-init-mu4e ()
  (with-eval-after-load 'mu4e

    (setq mu4e-bookmarks
          '((:name "inbox" :key ?i :query "maildir:/home/inbox")
            (:name "Unread messages" :query "(not \"maildir:/home/[Gmail]/Trash\") and flag:unread" :key ?u)
            (:name "Today's messages" :query "(not \"maildir:/home/[Gmail]/Trash\") and date:today..now" :key ?t)
            (:name "Today's everything" :query "date:today..now" :key ?T)
            (:name "Last 7 days" :query "(not \"maildir:/home/[Gmail]/Trash\") AND date:7d..now" :hide-unread t :key ?w)
            (:name "Messages with images" :query "mime:image/*" :key ?p)))

    (setq
          mu4e-get-mail-command "true"
          mu4e-view-use-gnus nil
          mu4e-update-interval nil
          mu4e-hide-index-messages t
          mu4e-index-update-in-background t
          mu4e-headers-results-limit -1
          mu4e-enable-async-operations t
          mu4e-compose-signature-auto-include nil
          mu4e-view-show-images t
          mu4e-view-show-addresses t
          mu4e-enable-notifications nil
          mu4e-enable-mode-line t
          mu4e-headers-skip-duplicates t
          ;; rename files when moving, needed for mbsync
          mu4e-change-filenames-when-moving t
          mu4e-compose-dont-reply-to-self t
          mu4e-compose-format-flowed t
          fill-flowed-encode-column 5000
          mu4e-compose-complete-only-personal t
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
          mail-envelope-from 'header
          mail-user-agent 'mu4e-user-agent)

    (defun mu4e-message-maildir-matches (msg rx)
      (string-match rx (mu4e-message-field msg :maildir)))

    (setq
     mu4e-context-policy 'pick-first
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
                 (smtpmail-smtp-user . "to.plotnick@gmail.com")
                 (message-signature-file . "~/.mail/home/.signature")
                 (mu4e-compose-signature . (concat "Thanks,\n" "Ag\n"))))
       ;; ,(make-mu4e-context
       ;;   :name "work"
       ;;   :enter-func (lambda () (mu4e-message "Switch to work"))
       ;;   ;; leave-func not defined
       ;;   :match-func (lambda (msg) (when msg (mu4e-message-maildir-matches msg "^/work/")))
       ;;   :vars '((smtpmail-smtp-server . "smtp.office365.com")
       ;;           (mu4e-sent-folder . "/work/Sent Items")
       ;;           (mu4e-trash-folder . "/work/Deleted Items")
       ;;           (mu4e-refile-folder . "/work/Archive")
       ;;           (mu4e-drafts-folder . "/work/Drafts")
       ;;           (user-mail-address . "ag@dividendfinance.com")
       ;;           (user-full-name . "Ag Ibragimov")
       ;;           (smtpmail-smtp-user . "ag.ibragimov@dividendfinance.com")
       ;;           (message-signature-file . "~/.mail/work/.signature")
       ;;           (mu4e-compose-signature . (concat "-----\nAg Ibragimov\nSoftware Developer | Dividend Finance\ndividendnow.com"))))
       ))

    (add-hook 'mu4e-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'mu4e-compose-mode-hook #'spacemacs/toggle-visual-line-navigation-on)
    (add-hook 'mu4e-compose-mode-hook #'flyspell-mode)
    (add-hook 'mu4e-view-mode-hook 'mu4e-prepare-view)
    (add-hook 'mu4e-compose-mode-hook 'mu4e-prepare-view)

    (setq mu4e-view-actions
          '(("capture message" . mu4e-action-capture-message)
            ("xwidget" . mu4e-action-view-with-xwidget)
            ("view in browser" . mu4e-action-view-in-browser)
            ("show this thread" . mu4e-action-show-thread)
            ("View as pdf" . mu4e-action-view-as-pdf)
            ("find in mailing-list" . mu4e-action-find-in-mailing-list)
            ("open in Gmail" . mu4e-action-open-in-gmail)))

    (add-to-list 'mu4e-headers-actions '("find in mailing-list" . mu4e-action-find-in-mailing-list))
    (add-to-list 'mu4e-headers-actions '("open in Gmail" . mu4e-action-open-in-gmail))

    (add-to-list
     'mu4e-header-info-custom
     '(:foldername . (:name "Folder information"
                            :shortname "Folder"
                            :help "Message short storage information"
                            :function (lambda (msg)
                                        (let ((shortaccount)
                                              (maildir (or (mu4e-message-field msg :maildir) "")))
                                          (when (not (string= maildir ""))
                                            (setq shortaccount
                                                  (substring
                                                   (replace-regexp-in-string "^/\\(\\w+\\)/.*$" "\\1" maildir)
                                                   0 1))
                                            (setq maildir (replace-regexp-in-string ".*/\\([^/]+\\)$" "\\1" maildir))
                                            (if (> (length maildir) 15)
                                                (setq maildir (concat (substring maildir 0 14) "…")))
                                            (setq maildir (concat "[" shortaccount "] " maildir)))
                                          maildir)))))

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

    (add-to-list
     'mu4e-header-info-custom
     '(:useragent . (:name "User-Agent"
                           :shortname "UserAgt."
                           :help "Mail client used by correspondant"
                           :function ed/get-origin-mail-system-header)))

    ;; headers view should be at 20 percent of frame height
    (setq mu4e-headers-visible-lines (truncate (* (frame-height) 0.2)))
    (add-to-list
     'mu4e-header-info-custom
     '(:empty . (:name "Empty"
                       :shortname ""
                       :function (lambda (msg) "  "))))
    (setq mu4e-headers-fields
          '((:empty . 2)
            (:flags . 6)
            (:human-date . 12)
            (:size . 6)
            (:foldername . 15)
            (:mailing-list . 10)
            (:from-or-to . 30)
            (:subject . nil))
          mu4e-headers-show-thread t
          mu4e-headers-include-related nil
          mu4e-use-fancy-chars t
          ;; mu4e-headers-date-format "%a %d %b %Y %H:%M"
          ;; mu4e-headers-time-format "%H:%M"
          mu4e-view-fields '(:from :to :cc :subject :flags :date :maildir :mailing-list :tags :useragent :attachments :signature :decryption)
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

    (add-hook 'mu4e-view-mode-hook #'spacemacs/toggle-visual-line-navigation-on)
    ;; (mu4e-maildirs-extension)

    (add-hook 'mu4e-main-mode-hook #'ag-mail/set-mu4e-keys)
    ;; (add-hook 'mu4e-main-mode-hook (lambda () (mu4e-update-mail-and-index t)))

    (add-hook 'mu4e-headers-mode-hook #'ag-mail/set-mu4e-keys)
    (add-hook 'mu4e-view-mode-hook #'ag-mail/set-mu4e-keys)

    ;; increase font-size in messages
    (dolist (f '(mu4e-view-body-face mu4e-cited-1-face mu4e-cited-2-face mu4e-cited-3-face mu4e-cited-4-face mu4e-cited-5-face mu4e-cited-6-face mu4e-cited-7-face))
      (set-face-attribute f nil :height 1.2))

    (set-face-attribute 'mu4e-header-highlight-face nil :weight 'normal)
    (set-face-attribute 'mu4e-unread-face nil :weight 'normal)

    ;; (defun set-mu4e-index-update-parameter (x)
    ;;   (if-let ((mu4e-layout? (-some-> (get-current-persp)
    ;;                            (persp-name)
    ;;                            (string= "@Mu4e"))))
    ;;       (setq mu4e-index-update-in-background nil)
    ;;     (setq mu4e-index-update-in-background t)))

    ;; (add-hook 'persp-activated-functions 'set-mu4e-index-update-parameter)
    )

  (defun mu4e--confirm-empty-subject ()
    "Allow user to quit when current message subject is empty."
    (or (message-field-value "Subject")
        (yes-or-no-p "Really send without Subject? ")
        (keyboard-quit)))

  (add-hook 'message-send-hook #'mu4e--confirm-empty-subject))

(defun ag-mail/init-mu4e-thread-folding ()
  (use-package mu4e-thread-folding
    :commands (mu4e-main-mode)
    :init
    (require 'mu4e-thread-folding))
  )

(with-eval-after-load 'mu4e-alert
  ;; Enable Desktop notifications
  (mu4e-alert-set-default-style 'notifications))


;;; packages.el ends here
