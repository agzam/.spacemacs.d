(setq ag-hipchat-packages
    '(jabber))

(defun ag-hipchat/init-jabber ()
  (use-package jabber
  :defer t
  :init
  (evil-leader/set-key "a h s" 'jabber-connect-all)
  (evil-leader/set-key "a h c" 'jabber-chat-with)
  (evil-leader/set-key "a h r" 'jabber-display-roster)
  (evil-leader/set-key "a h a" 'jabber-activity-switch-to)

  (setq ssl-program-name "gnutls-cli"
        ssl-program-arguments '("--insecure" "-p" service host)
        ssl-certificate-verification-policy 1)

  (setq jabber-account-list '(("@chat.hipchat.com"
                               (:port . 5223)
                               (:password . "")
                               (:connection-type . ssl))))

  (add-hook 'jabber-chat-mode-hook 'flyspell-mode)

  ;; This enables notifications for OS X
  (defun msg-via-notifier (title msg) (shell-command (format "/usr/local/bin/terminal-notifier -sender org.gnu.Emacs -title '%s' -message '%s'" title msg)))
  (defun notify-jabber-message (from buf text proposed-alert) (msg-via-notifier from text))
  (add-hook 'jabber-alert-message-hooks 'notify-jabber-message)

  ;; this info is present on the hipchat xmpp info page
  (defvar hipchat-room-list '(("" . "")))

  ;; To join HipChat rooms easily
  (defvar hipchat-number "")
  (defvar hipchat-nickname "Ag Ibragimov")
  (defun hipchat-join ()
    (interactive)
    (let* ((room-list (sort (mapcar 'car hipchat-room-list) 'string-lessp))
           (selected-room (completing-read "Room name: " room-list))
           (hipchat-mapping (cdr (assoc selected-room hipchat-room-list))))
      (jabber-groupchat-join
       (jabber-read-account)
       (concat hipchat-number "" hipchat-mapping "@conf.hipchat.com")
       hipchat-nickname
       t)))

  (custom-set-faces
   '(jabber-activity-personal-face ((t (:foreground "tan3" :weight bold))))
   '(jabber-chat-prompt-local ((t (:foreground "DodgerBlue2" :weight bold))))
   '(jabber-rare-time-face ((t (:foreground "green4" :underline t))))
   '(jabber-roster-user-away ((t (:foreground "light steel blue" :slant italic :weight normal))))
   '(jabber-roster-user-online ((t (:foreground "chartreuse2" :slant normal :weight bold))))))

  (setq
   jabber-auto-reconnect t
   jabber-show-resources nil
   jabber-activity-count-in-title t
   jabber-history-enabled t
   jabber-use-global-history nil))
