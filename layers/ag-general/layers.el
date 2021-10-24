;;; layers.el --- layers required by ag-general layer.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <ag.ibragimov@C02MT2ZDFH05>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(configuration-layer/declare-layers '(
                                      ;; spacemacs-base
                                      spacemacs-defaults
                                      spacemacs-completion
                                      spacemacs-layouts
                                      spacemacs-editing
                                      spacemacs-editing-visual
                                      (spacemacs-evil
                                       :packages (not evil-exchange))
                                      spacemacs-language
                                      spacemacs-misc
                                      ;; spacemacs-modeline
                                      spacemacs-navigation
                                      spacemacs-org
                                      spacemacs-project
                                      ;; spacemacs-purpose
                                      spacemacs-visual
                                      ibuffer))

;;; layers.el ends here
