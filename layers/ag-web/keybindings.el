;;; keybindings.el --- ag-web layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(evil-define-key 'normal web-mode-map "zo" 'web-mode-fold-or-unfold)
(evil-define-key 'normal web-mode-map "zc" 'web-mode-fold-or-unfold)
(evil-leader/set-key-for-mode 'web-mode "mhh" 'web-mode-dom-xpath)
(evil-define-key 'normal web-mode-map "K" 'web-mode-dom-xpath)

;; ---------------
;; css-mode
;; ---------------
(evil-leader/set-key-for-mode 'css-mode "mhh" 'helm-css-scss)
(evil-define-key 'normal css-mode-map "K" 'helm-css-scss)

;; ------------
;; Javascript mode
;; ------------
(evil-define-key 'normal js2-mode-map "zc" 'js2-mode-hide-element)
(evil-define-key 'normal js2-mode-map "zo" 'js2-mode-show-element)
(evil-leader/set-key-for-mode 'js2-mode "mhh" 'tern-get-docs)

;;; keybindings.el ends here
