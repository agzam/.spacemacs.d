;;; keybindings.el --- ag-version-control layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;;; l and h are for navigating. even in magit
(with-eval-after-load 'magit
  (evil-define-key evil-magit-state magit-mode-map "l" 'evil-forward-char)
  (evil-define-key evil-magit-state magit-mode-map (kbd "M-l") 'magit-log)
  (evil-define-key evil-magit-state magit-mode-map "h" 'evil-backward-char)
  (evil-define-key evil-magit-state magit-mode-map (kbd "M-h") 'magit-dispatch))

;; (with-eval-after-load 'forge
;; (evil-magit-define-key evil-magit-state 'magit-mode-map "'" 'forge-dispatch))

(spacemacs/set-leader-keys
  "gft" #'magit-log-trace-definition)

;;; keybindings.el ends here
