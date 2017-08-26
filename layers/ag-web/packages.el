(setq ag-web-packages '())

(with-eval-after-load 'css-mode
  (defun ag/rainbow-mode-on () (rainbow-mode 1))
  (add-hook 'css-mode-hook #'ag/rainbow-mode-on)
  (add-hook 'scss-mode-hook #'ag/rainbow-mode-on)

  (setq css-indent-offset 2
        sass-indent-offset 2))

(with-eval-after-load 'js2-mode
  (setq js2-include-node-externs t
        js2-include-browser-externs t
        js2-include-global-externs t
        js2-global-externs (list "$" "window" "angular" "Rx" "_" "moment")
        coffee-tab-width 4
        js-indent-level 2
        js2-basic-offset 2
        )
  (add-hook 'js2-mode-hook #'flyspell-prog-mode)

  ;; Flycheck JSCS
  (flycheck-def-config-file-var flycheck-jscs
      javascript-jscs ".jscs.json" :safe #'stringp)
  (flycheck-define-checker javascript-jscs
    "A JavaScript code style checker. See URL `https://github.com/mdevils/node-jscs'."
    :command ("jscs" "--reporter"
              "checkstyle"
              (config-file "--config" flycheck-jscs)
              source):error-parser
              flycheck-parse-checkstyle
              :modes (js-mode js2-mode js3-mode):next-checkers
              (javascript-jshint))
  (defun jscs-enable ()
    (interactive)
    (add-to-list 'flycheck-checkers 'javascript-jscs))
  (defun jscs-disable ()
    (interactive)
    (setq flycheck-checkers (remove 'javascript-jscs flycheck-checkers)))
  (setq js2-strict-inconsistent-return-warning nil)

  ;; Flycheck coffeelint
  (setq flycheck-coffee-coffeelint-executable
        "~/.nvm/versions/node/v5.6.0/bin/node/bin/coffeelint")
  (setq flycheck-coffeelintrc "~/.coffeelintrc")
  (add-hook 'coffee-mode-hook #'flyspell-prog-mode)

  ;; (setenv "PATH" (concat (getenv "PATH") ":~/.nvm/versions/node/v5.6.0/bin/node"))
  ;; (setq exec-path (append exec-path '("~/.nvm/versions/node/v5.6.0/bin/node/bin")))
  ;; get rid of stupid tern-port files
  (setq tern-command '("tern" "--no-port-file"))

  (add-to-list 'sp-sexp-suffix '(js2-mode regex "")))

(setq flycheck-stylelintrc ".stylelintrc.yml")
