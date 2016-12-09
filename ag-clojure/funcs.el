(defun switch-to-nrepl-window (&optional PROMT-PROJECT CLJS-TOO)
  (save-selected-window)
  (let* ((nrepl-buf (nrepl-make-buffer-name
                     nrepl-server-buffer-name-template
                     (clojure-project-dir (cider-current-dir)) nil nil -1)))
    (when (not (equal (buffer-name) nrepl-buf)) 
      (switch-to-buffer-other-window nrepl-buf))))

(with-eval-after-load 'cider
  (advice-add 'cider-jack-in :after #'switch-to-nrepl-window)
  ;; (add-hook 'cider-connected-hook (lambda ()
  ;;                                   (save-selected-window
  ;;                                     (switch-to-nrepl-window)
  ;;                                     (split-window-below-and-focus)
  ;;                                     (switch-to-buffer (cider-current-repl-buffer)))))
  )

(defun ag/clojars-find ()
  "Lookup for symbol at point on clojars. Useful for updating packages in project.clj"
  (interactive)
  (let ((s (symbol-at-point)))
    (when s (clojars s))))

(defun cljr-toggle-ignore-form ()
  "clojure - ignore (comment) form"
  (interactive)
  (if (search-backward "#_" 2 t 1)
      (delete-char 2)
    (progn
      (let ((fc (following-char)))
        (cond ((-contains? '( ?\) ?\] ?\} ) fc) (paredit-backward-up))
              ((-contains? '( ?\( ?\[ ?\: ?\{ ) fc) nil)
              (t (beginning-of-thing 'sexp)))
        (insert "#_")))))

(defun project-clj-hook ()
  "for project.clj files"
  (if (and (stringp buffer-file-name)
           (string-match "\\project.clj|build.boot\\'" buffer-file-name))
      (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "c" 'ag/clojars-find)
    (spacemacs/set-leader-keys-for-major-mode 'clojure-mode "c" nil)))

;; (with-eval-after-load 'bind-map (add-hook 'find-file-hook 'project-clj-hook))
