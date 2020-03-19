;;; keybindings.el --- ag-clojure layer keybindings.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(with-eval-after-load 'cider
  (spacemacs|forall-clojure-modes m
    (spacemacs/set-leader-keys-for-major-mode m
      "fl" 'clojure-align
      "sl" (lambda () (interactive) (cider-find-and-clear-repl-output t))
      ";"   #'cljr-toggle-ignore-form
      "hh" #'cider-clojuredocs
      "'"   #'cider-switch-to-repl-buffer
      "ep" #'cider-pprint-eval-last-sexp-to-comment
      "ss" (if (eq m 'cider-repl-mode)
               'cider-switch-to-last-clojure-buffer
             'cider-switch-to-repl-buffer)))

  (spacemacs/set-leader-keys-for-major-mode 'clojurescript-mode
    "g f" #'re-frame-jump-to-reg))

(with-eval-after-load 'lsp-mode
 (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references))

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "C" 'clojars-find)

;;; keybindings.el ends here
