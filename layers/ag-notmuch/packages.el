;;; packages.el --- ag-notmuch layer packages
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
(defconst ag-notmuch-packages '((notmuch :location (recipe :fetcher git
                                                           :url "git://git.notmuchmail.org/git/notmuch"
                                                           :files ("emacs/*.el")))
                                (counsel-notmuch :requires ivy)
                                org
                                persp-mode
                                ;; notmuch-labeler
                                notmuch-maildir
                                ))

(defun ag-notmuch/init-counsel-notmuch ()
  (use-package counsel-notmuch
    :defer t
    :init (spacemacs/set-leader-keys "aee" 'counsel-notmuch)))


(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-queue-mail nil
      smtpmail-queue-dir "~/.mail/queue/cur"
      ;; send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      ;; mail-specify-envelope-from t
      ;; mail-envelope-from 'header

      smtpmail-smtp-user "agzam.ibragimov@gmail.com"
      smtpmail-mail-address "agzam.ibragimov@gmail.com"
      user-mail-address "agzam.ibragimov@gmail.com"
      smtpmail-debug-info t
      smtpmail-debug-verb t
      auth-source-debug t
      auth-source-do-cache nil)

(defun ag-notmuch/init-notmuch ()
  (use-package notmuch
    :defer t
    :after (evil evil-escape)
    :commands notmuch
    :init
    (progn
      (spacemacs/declare-prefix "ae" "notmuch")
      (spacemacs/set-leader-keys
        "aen" 'notmuch
        "aei" 'spacemacs/notmuch-inbox
        "aej" 'notmuch-jump-search
        "aes" 'notmuch-search))
    :config
    (setq notmuch-search-oldest-first nil
          message-confirm-send t

          ;; needed so lsp-grammarly works
          message-auto-save-directory "/tmp"
          mm-text-html-renderer 'gnus-w3m)

    (dolist (m '(notmuch-search-mode
                 notmuch-tree-mode
                 notmuch-show-mode))
      (add-to-list 'evil-escape-excluded-major-modes m))

    (dolist (prefix '(("ms" . "stash")
                      ("mp" . "part")
                      ("mP" . "patch")))
      (spacemacs/declare-prefix-for-mode 'notmuch-show-mode
        (car prefix) (cdr prefix)))
    ;; key bindings
    (evil-define-key 'visual notmuch-search-mode-map
      "*" 'notmuch-search-tag-all
      "a" 'notmuch-search-archive-thread
      "-" 'notmuch-search-remove-tag
      "+" 'notmuch-search-add-tag)
    (spacemacs/set-leader-keys-for-major-mode 'notmuch-show-mode
      "a" 'notmuch-show-save-attachments
      ;; part
      "pm" 'notmuch-show-choose-mime-of-part
      "pp" 'spacemacs/notmuch-show-as-patch
      "p|" 'notmuch-show-pipe-part
      "po" 'notmuch-show-interactively-view-part
      "pv" 'notmuch-show-view-part
      "ps" 'notmuch-show-save-part
      ;; stash
      "sG" 'notmuch-show-stash-git-send-email
      "sL" 'notmuch-show-stash-mlarchive-link-and-go
      "sl" 'notmuch-show-stash-mlarchive-link
      "st" 'notmuch-show-stash-to
      "sT" 'notmuch-show-stash-tags
      "ss" 'notmuch-show-stash-subject
      "sI" 'notmuch-show-stash-message-id-stripped
      "si" 'notmuch-show-stash-message-id
      "sf" 'notmuch-show-stash-from
      "sF" 'notmuch-show-stash-filename
      "sd" 'notmuch-show-stash-date
      "sc" 'notmuch-show-stash-cc
      ;; patch
      "Po" 'spacemacs/notmuch-show-open-github-patch
      "Pa" 'spacemacs/notmuch-git-apply-patch
      "PA" 'spacemacs/notmuch-git-apply-patch-part
      "fg" #'notmuch-open-in-gmail
      "fl" #'notmuch-find-in-mailing-list)
    ;; Evilify notmuch modes
    ;; Use normal mode map to allow proper editing capabilities
    ;; for the embedded search field in `notmuch-hello-mode`
    (evil-set-initial-state 'notmuch-hello-mode 'normal)
    (evil-define-key 'normal notmuch-hello-mode-map
      "C-tab" #'widget-backward
      "S-tab" #'widget-backward
      "=" #'notmuch-refresh-this-buffer
      "?" #'notmuch-help
      "G" #'notmuch-poll-and-refresh-this-buffer
      "g" #'notmuch-refresh-this-buffer
      "J" #'notmuch-jump-search
      "m" #'notmuch-mua-new-mail
      "q" #'notmuch-bury-or-kill-this-buffer
      "s" #'notmuch-search
      "v" #'notmuch-hello-versions
      "z" #'notmuch-tree
      "M-=" #'notmuch-refresh-all-buffers)
    ;; Make notmuch message mode closable via q
    (evil-define-key 'normal notmuch-message-mode-map
      "q" #'message-kill-buffer)
    (evilified-state-evilify-map notmuch-show-mode-map
      :mode notmuch-show-mode
      :bindings
      (kbd "d") #'notmuch-show-delete
      (kbd "C-n")  #'notmuch-show-next-message
      (kbd "C-p")  #'notmuch-show-previous-message
      (kbd "p")   'notmuch-show-previous-open-message
      (kbd "o")   'notmuch-show-open-or-close-all
      (kbd "O")   'spacemacs/notmuch-show-close-all
      (kbd "{")   #'evil-backward-paragraph
      (kbd "}")   #'evil-forward-paragraph
      (kbd "[[")   #'evil-backward-section-begin
      (kbd "]]")   #'evil-forward-section-end)
    (evilified-state-evilify-map notmuch-tree-mode-map
      :mode notmuch-tree-mode
      :bindings
      (kbd "N") 'notmuch-tree-next-message
      (kbd "P") 'notmuch-tree-prev-message
      (kbd "d") #'notmuch-tree-toggle-delete
      (kbd "D") (lambda ()
                  (interactive)
                  (notmuch-tree-toggle-delete :up))
      (kbd "n") 'notmuch-tree-next-matching-message
      (kbd "p") 'notmuch-tree-prev-matching-message
      (kbd "M-d") 'notmuch-tree-scroll-message-window
      (kbd "M-u") 'notmuch-tree-scroll-message-window-back
      (kbd "M") 'compose-mail-other-frame)
    (evilified-state-evilify-map notmuch-search-mode-map
      :mode notmuch-search-mode
      :bindings
      (kbd "a") 'spacemacs/notmuch-search-archive-thread-down
      (kbd "A") 'spacemacs/notmuch-search-archive-thread-up
      (kbd "d") 'spacemacs/notmuch-search-message-delete-down
      (kbd "D") 'spacemacs/notmuch-search-message-delete-up
      (kbd "J") 'notmuch-jump-search
      (kbd "L") 'notmuch-search-filter
      (kbd "gg") 'notmuch-search-first-thread
      (kbd "gr") 'notmuch-refresh-this-buffer
      (kbd "gR") 'notmuch-refresh-all-buffers
      (kbd "G") 'notmuch-search-last-thread
      (kbd "M") 'compose-mail-other-frame)

    (add-hook 'notmuch-message-mode-hook #'spacemacs/toggle-auto-fill-mode-off)
    (add-hook 'notmuch-show-hook #'spacemacs/toggle-centered-buffer)

    (defun notmuch-tree-show-message--switch-to-message-window (arg)
      (setq notmuch-last-tree-window (selected-window))
      (select-window notmuch-tree-message-window))

    (advice-add 'notmuch-tree-show-message :after #'notmuch-tree-show-message--switch-to-message-window)))

(defun ag-notmuch/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (require 'ol-notmuch)))

(defun ag-notmuch/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      (add-to-list 'persp-filter-save-buffers-functions
                   'spacemacs//notmuch-persp-filter-save-buffers-function)
      (spacemacs|define-custom-layout notmuch-spacemacs-layout-name
        :binding notmuch-spacemacs-layout-binding
        :body
        (progn
          (dolist (mode notmuch-modes)
            (let ((hook (intern (concat (symbol-name mode) "-hook"))))
              (add-hook hook #'spacemacs//notmuch-buffer-to-persp)))
          (call-interactively 'notmuch))))))

(defun ag-notmuch/init-notmuch-labeler ()
  (use-package notmuch-labeler
    :defer t
    :after notmuch
    )
  )

(defun ag-notmuch/init-notmuch-maildir ()
  (use-package notmuch-maildir
    :config
    (notmuch-maildir-inject-section)
    ))
;;; packages.el ends here
