;;; packages.el --- ag-slack layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-slack-packages '())

(with-eval-after-load 'slack
  (setq lui-time-stamp-format "[%b-%d %H:%M:%S]"
        lui-time-stamp-only-when-changed-p t
        lui-time-stamp-position 'right
        lui-fill-column 80

        slack-buffer-emojify t
        slack-prefer-current-team t
        alert-fade-time 20
        alert-persist-idle-time 15
        alert-default-style 'mode-line
        websocket-callback-debug-on-error t)
  (defun enable-company-slack-backend ()
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends '(company-slack-backend :with company-ispell)))
  (defun customize-slack-mode ()
    (enable-company-slack-backend)
    (setq tab-width 2))
  (add-hook 'slack-edit-message-mode-hook 'enable-company-slack-backend)
  (add-hook 'slack-mode-hook 'customize-slack-mode))
