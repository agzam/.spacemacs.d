(dolist (x '(spacemacs-jump-handlers-clojure-mode
             spacemacs-jump-handlers-clojurec-mode
             spacemacs-jump-handlers-clojurescript-mode
             spacemacs-jump-handlers-clojurex-mode
             spacemacs-jump-handlers-cider-repl-mode))
  (add-to-list x 'dumb-jump-go))
