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
      "fl" #'clojure-align
      "fL" #'clojure-unalign
      "sl" (lambda () (interactive) (cider-find-and-clear-repl-output t))
      ";"   #'cljr-toggle-ignore-form
      "'"   #'cider-switch-to-repl-buffer
      "ep" #'cider-pprint-eval-last-sexp-to-comment
      "ss" (if (eq m 'cider-repl-mode)
               'cider-switch-to-last-clojure-buffer
             'cider-switch-to-repl-buffer)
      "ii" #'clj-fully-qualified-symbol-at-point
      "ir" #'clojure-add-require
      "tl" nil ; keep accidentally pressing, triggering hundreds of tests to run in the REPL
      "r-" #'clojure-toggle-ignore
      "sl" #'spacemacs/cider-find-and-clear-repl-buffer*
      "sS" #'cider-switch-to-nrepl-buffer))

  (spacemacs/set-leader-keys-for-major-mode 'clojurescript-mode
    "g f" #'re-frame-jump-to-reg)

  (set-slurp-n-barf-keys '(clojure-mode-map cider-repl-mode-map)))

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "C" 'clojars-find)

(evil-define-key 'normal cider-test-report-mode-map "n" nil) ; it otherwise gets shadowed by cider key

;;; keybindings.el ends here
