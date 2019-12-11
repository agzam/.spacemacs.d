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
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode
               cider-repl-mode))
    (spacemacs/set-leader-keys-for-major-mode m
      ";"   #'cljr-toggle-ignore-form
      "h h" #'helm-clojuredocs-at-point
      "'"   #'cider-switch-to-repl-buffer
      "e p" #'cider-pprint-eval-last-sexp-to-comment))

  (spacemacs/set-leader-keys-for-major-mode 'clojurescript-mode
    "g f" #'re-frame-jump-to-reg))

(with-eval-after-load 'lsp-mode
 (define-key lsp-mode-map [remap xref-find-references] #'lsp-find-references))

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "C" 'clojars-find)

;;; keybindings.el ends here
