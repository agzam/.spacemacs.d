;;; funcs.el --- ag-clojure layer functions file.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun switch-to-nrepl-window (&optional PROMT-PROJECT CLJS-TOO)
  (save-selected-window)
  (let* ((nrepl-buf (nrepl-make-buffer-name
                     nrepl-server-buffer-name-template
                     (clojure-project-dir (cider-current-dir)))))
    (when (not (equal (buffer-name) nrepl-buf))
      (switch-to-buffer-other-window nrepl-buf))))

(defun clojars-find ()
  "Lookup for symbol at point on clojars. Useful for updating packages in project.clj"
  (interactive)
  (clojars (symbol-at-point)))

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

(defun clj-fully-qualified-symbol-at-point (&optional for-req)
  "Gets fully qualified Clojure symbol at point. If FOR-REQ argument passed
gets the name suitable for :require of ns declaration."
  (interactive "P")
  (flet ((use-results (x)
                      (message x)
                      (kill-new x)
                      x))
    (let ((sym (cond ((lsp--capability :hoverProvider)
                      (let ((s (-some->> (lsp--text-document-position-params)
                                 (lsp--make-request "textDocument/hover")
                                 (lsp--send-request)
                                 (gethash "contents")
                                 (gethash "value"))))
                        (string-match "\\(```.*\n\\)\\(.*\\)\n" s)
                        (string-trim (match-string 2 s))))

                     ((cider-connected-p)
                      (let ((cb (lambda (x)
                                  (when-let ((v (nrepl-dict-get x "value"))
                                             (s (replace-regexp-in-string "[()]" "" v)))
                                    (message (string-trim s))
                                    (kill-new s)))))
                        (cider-interactive-eval
                         (concat "`(" (cider-symbol-at-point t) ")")
                         cb)))
                     (t (message "Neither lsp nor cider are connected")))))
      (if for-req  ; want ns header name, e.g.: "[foo.core :as foo]"
          (if-let* ((m (string-match "^\\(.*\\)\\/" sym))) ; attempt to get anything before the slash
              (let* ((suffix (match-string 1 sym))  ; grab suffix of the symbol i.e. 'foo.core' of 'foo.core/my-thing'
                     ;; grep for '[foo.core :as ...' in the project
                     (grepped (string-trim
                               (shell-command-to-string
                                (format
                                 "rg --glob '*.clj' --max-count 1 --no-filename '\\[%s :as' %s"
                                 suffix
                                 (projectile-project-root))))))
                (if-let* ((m (string-match "\\[.*\\]" grepped))
                          (res (match-string 0 grepped)))
                    (use-results res)
                  (use-results (format "[%s :as ]" suffix))))
            (use-results sym))
        (use-results sym)))))

(defun re-frame-jump-to-reg ()
  "Borrowed from https://github.com/oliyh/re-jump.el"
  (interactive)
  (let* ((kw (cider-symbol-at-point 'look-back))
         (ns-qualifier (and
                        (string-match "^:+\\(.+\\)/.+$" kw)
                        (match-string 1 kw)))
         (kw-ns (if ns-qualifier
                    (cider-resolve-alias (cider-current-ns) ns-qualifier)
                  (cider-current-ns)))
         (kw-to-find (concat "::" (replace-regexp-in-string "^:+\\(.+/\\)?" "" kw))))

    (when (and ns-qualifier (string= kw-ns (cider-current-ns)))
      (error "Could not resolve alias \"%s\" in %s" ns-qualifier (cider-current-ns)))

    (progn (cider-find-ns "-" kw-ns)
           (search-forward-regexp (concat "reg-[a-zA-Z-]*[ \\\n]+" kw-to-find) nil 'noerror))))

(defun add-reframe-regs-to-imenu ()
  (add-to-list
   'imenu-generic-expression
   '("re-frame" "(*reg-\\(event-db\\|sub\\|sub-raw\\|fx\\|event-fx\\|event-ctx\\|cofx\\)[ \n]+\\([^\t \n]+\\)" 2)
   t))

(defun cljr-ns-align ()
  "Align ns requires."
  (interactive)
  (end-of-buffer)
  (when (re-search-backward "^\(ns.*\\(\n.*\\)*\(:require" nil t nil)
    (mark-sexp)
    (align-regexp (region-beginning)
                  (region-end)
                  "\\(\\s-*\\)\\s-:")))

(defun kill-cider-buffers ()
  "Kill all CIDER buffers without asking any questions. Useful to execute when Emacs gets stuck."
  (interactive)
  (flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
    (let ((kill-buffer-query-functions
           (delq 'process-kill-buffer-query-function kill-buffer-query-functions))))
    (kill-matching-buffers "cider")))

(defun format-edn ()
  "Formats edn without cider"
  (interactive)
  (let ((start (when mark-active (region-beginning)))
        (end (when mark-active (region-end))))
    (let ((jet (executable-find "jet")))
      (call-process-region
       start end jet
       :delete '(t nil)
       :display "--pretty"))))

(defun clojure-unalign (beg end)
  "Un-align (remove extra spaces) in vertically aligned sexp around the point."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (let ((end (progn (end-of-defun)
                                     (point))))
                     (clojure-backward-logical-sexp)
                     (list (point) end)))))

  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " "))
      (let ((clojure-align-forms-automatically nil))
       (indent-region beg end)))))

(defun clojure-map-sort (beg end)
  "Sort a clojure map"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (save-excursion
                   (when (looking-at "\{")
                     (forward-char))
                   (let ((end (progn (sp-end-of-sexp)
                                     (point))))
                     (sp-beginning-of-sexp)
                     (backward-char)
                     (list (point) (+ 1 end))))))
  (let ((zprint (executable-find "zprint")))
    (save-excursion
      (call-process-region
       beg
       end
       zprint
       :delete
       '(t nil)
       :display
       "{:map {:comma? false :justify? true}}")
      (sp-reindent))))

(defun clojure-edn-json-transform (&optional from-json)
  "Transforms EDN to JSON and vice-versa using jet cli.
The direction is determined by current major-mode or can be
explicitly set by universal argument, if present - attemps to
convert from JSON."
  (interactive "P")
  (let* ((from-json* (or from-json (eq major-mode 'json-mode)))
         (region (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (save-excursion
                     (when (looking-at "\{")
                       (forward-char))
                     (let ((end (progn (sp-end-of-sexp)
                                       (point))))
                       (sp-beginning-of-sexp)
                       (backward-char)
                       (list (point) (+ 1 end))))))
         (jet (executable-find "jet"))
         (params (if from-json*
                     '("--from" "json" "--to" "edn" "--keywordize" "--pretty")
                   '("--from" "edn" "--to" "json" "--pretty"))))
    (when (not jet)
      (error "jet cli not found"))
    (save-excursion
      (apply 'call-process-region
             (car region)
             (cadr region)
             jet
             :delete
             '(t nil)
             :display
             params)
      (if (or (eq major-mode 'json-mode) from-json*)
          (clojure-mode)
        (json-mode))
      (sp-reindent))))

(defun clojure-add-require (&optional req-str)
  "Adds a ns import (require) line to Clojure ns header.
If REQ-STR, is provided, uses that, otherwise tries to grab one
from the kill-ring.

REQ-STR (or the last item in the kill-ring) is expected to be of
the form: [foo.core :as [foo]].
"
  (interactive)
  (when-let* ((req-str* (or req-str (current-kill 0 t)))
              (match? (string-match-p "^\\[[[:alpha:]]*.*\\]$" req-str*)))
    (goto-char 0)
    (search-forward ":require")
    (sp-end-of-sexp)
    (insert "\n")
    (insert req-str*)
    (sp-reindent)
    (clojure-sort-ns)
    (goto-char 0)
    (sp-end-of-next-sexp)
    (forward-char)
    (cider-eval-last-sexp)
    ;; TODO: add a check for when require statement already exists
    ;; TODO: highlight newly added require
    ))

;; redefine clear-repl-buffer, so it also clears the nrepl buffer
(defun spacemacs/cider-find-and-clear-repl-buffer* ()
  "Calls cider-find-and-clear-repl-output interactively with C-u prefix
set so that it clears the whole REPL buffer, not just the output."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'cider-find-and-clear-repl-output)
    (when-let ((nrepl-buf (nrepl-make-buffer-name
                      (nrepl--make-hidden-name nrepl-server-buffer-name-template)
                      nil :no-dup)))
      (set-buffer nrepl-buf)
      (comint-clear-buffer))))

(defun cider-switch-to-nrepl-buffer ()
  "Calls cider-find-and-clear-repl-output interactively with C-u prefix
set so that it clears the whole REPL buffer, not just the output."
  (interactive)
  (when-let ((nrepl-buf (nrepl-make-buffer-name
                         (nrepl--make-hidden-name nrepl-server-buffer-name-template)
                         nil :no-dup)))
    (switch-to-buffer-other-window nrepl-buf)))

;;; funcs.el ends here
