;;; packages.el --- ag-web layer packages
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst ag-web-packages '(prettier-js
                            ;; js2-mode
                            ))

(defun ag-web/init-prettier-js ()
  (use-package prettier-js
    :commands prettier-js
    :init
    (dolist (mode '(js2-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "fl" #'prettier-js))))

;; (defun ag-web/post-init-js2-mode ()
;;   ;; for some reason js2-mode-hide-element, etc. not working - using alternative
;;   (spacemacs/set-leader-keys-for-major-mode 'js2-mode
;;     "zc" 'hs-hide-block
;;     "zo" 'hs-show-block
;;     "zr" 'hs-show-all
;;     "ze" 'hs-toggle-hiding))

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
        js2-strict-trailing-comma-warning nil)
  ;; (add-hook 'js2-mode-hook #'flyspell-prog-mode)

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

(with-eval-after-load 'scss
  (add-hook 'scss-mode-hook
            (lambda ()
              (set (make-local-variable 'compile-command)
                   (concat "stylelint --fix " buffer-file-name))))

  ;;;; Overriding scss-mode.el implementation
  (defun scss-compile()
    "Compiles the directory belonging to the current buffer, using the --update option"
    (interactive)
    (compile (concat "stylelint --fix " buffer-file-name))))

;;; packages.el ends here
