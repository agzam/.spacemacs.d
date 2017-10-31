(with-eval-after-load 'cider
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode
               cider-repl-mode))
    (spacemacs/set-leader-keys-for-major-mode m
      ";" 'cljr-toggle-ignore-form
      "h h" 'helm-clojuredocs-at-point
      "'" 'cider-switch-to-repl-buffer)))

(spacemacs/set-leader-keys-for-major-mode 'clojure-mode
  "C" 'clojars-find)
