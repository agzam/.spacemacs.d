(setq ag-dash-packages '(helm-dash))

(defun ag-dash/init-helm-dash ()
  (use-package helm-dash
    :defer t
    :init
    (setq helm-dash-browser-func 'eww)

    (defun js-dash-doc ()
      (interactive)
      (setq-local helm-dash-docsets '("Lo-Dash")))
    (add-hook 'js2-mode-hook 'js-dash-doc)

    (defun css-dash-doc ()
      (interactive)
      (setq-local helm-dash-docsets '("CSS" "Bootstrap_3" "Bootstrap_4")))
    (add-hook 'css-mode-hook 'css-dash-doc)
    (add-hook 'web-mode-hook 'css-dash-doc)

    (defun clojure-dash-doc ()
      (interactive)
      (setq-local helm-dash-docsets '("Clojure")))
    (add-hook 'clojure-mode-hook 'clojure-dash-doc)

    (evil-leader/set-key
      "dh" 'helm-dash-at-point
      "dH" 'helm-dash)

    :config))


