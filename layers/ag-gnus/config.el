;;; config.el --- ag-gnus layer configuration
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
;; (setq user-mail-address "agzam.ibragimov@gmail.com"
;;       user-full-name "Ag Ibragimov")

(with-eval-after-load 'gnus
  (add-to-list 'gnus-secondary-select-methods '(nntp "news.eternal-september.org"))

  (setq shr-use-colors nil)

  (setq user-mail-address "agzam.ibragimov@gmail.com"
        user-full-name "Ag Ibragimov")

  (setq gnus-select-method
        '(nnimap "gmail"
                 (nnimap-address "imap.gmail.com")
                 (nnimap-server-port "imaps")
                 (nnimap-stream ssl)))

  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

  (unbind-key (kbd "\ek") gnus-group-mode-map)
  (unbind-key (kbd "\ek") gnus-summary-mode-map)
  ;; (setq user-full-name "Ag Ibragimov"
  ;;       user-mail-address "agzam.ibragimov@gmail.com")

  ;; (add-to-list 'gnus-secondary-select-methods
  ;;              '(nnimap "personal"
  ;;                       (nnimap-stream network)
  ;;                       (nnimap-address "localhost")
  ;;                       (nnimap-server-port 8143)
  ;;                       (nnimap-authenticator login)))

  ;; (add-to-list 'gnus-secondary-select-methods
  ;;              '(nnimap "work"
  ;;                       (nnimap-stream network)
  ;;                       (nnimap-address "localhost")
  ;;                       (nnimap-server-port 8144)
  ;;                       (nnimap-authenticator login)))

  ;; (setq gnus-parameters
  ;;       '(("nnimap work:INBOX"
  ;;          (display . all)
  ;;          (posting-style
  ;;           (name "Ag Ibragimov")
  ;;           (address "ag.ibragimov@fundingcircle.com")
  ;;           (organization "Funding Circle")
  ;;           ;; (signature-file "~/.signature-work")
  ;;           )
  ;;          (expiry-target . delete)
  ;;          ("nnimap work:[Gmail]/.*"
  ;;           (display . all)
  ;;           (posting-style
  ;;            (name "Ag Ibragimov")
  ;;            (address "ag.ibragimov@fundingcircle.com")
  ;;            (organization "Funding Circle")
  ;;            ;; (signature-file "~/.signature-work")
  ;;            )
  ;;           (expiry-wait . never))
  ;;          ("nnimap home:(INBOX|lists..*)"
  ;;           (display . all)
  ;;           (posting-style
  ;;            (name "Ag Ibragimov")
  ;;            (address "agzam.ibragimov@gmail.com")
  ;;            ;; (signature-file "~/.signature-home")
  ;;            )
  ;;           (expiry-target . delete)
  ;;           ("nnimap home:[Gmail]/.*"
  ;;            (display . all)
  ;;            (posting-style
  ;;             (name "Ag Ibragimov")
  ;;             (address "agzam.ibragimov@gmail.com")
  ;;             ;; (signature-file "~/.signature-home")
  ;;             )
  ;;            (expiry-wait . never))))))
  )

;;; config.el ends here
