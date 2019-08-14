;;; jira.el --- Jira utils -*- lexical-binding: t; -*-
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/dot-spacemacs/tree/master/layers/ag-general/local/jira/jira.el
;; Created: 2019-08
;; Keywords: jira, project
;; License: GPL v3
;; Version: 0.0.1

;;; Commentary:
(defvar jira-base-url "" "Jira base url, e.g: https://jira.mycompany.com")
(defvar jira-project "scrum-" "default Jira project prefix")

(defun jira--get-ticket-details (ticket-number)
  "Attempts to run go-jira cli for given TICKET-NUMBER. If
successful, returns hashmap.

Returns nil, if Jira CLI fails for any reason: auth error,
non-existent ticket, etc."
  (ignore-errors
    (json-read-from-string
     (shell-command-to-string
      (concat "jira view " ticket-number " --template=debug")))))

(defun jira--get-ticket-summary (ticket-number)
  "Tries to retrieve summary for a given Jira TICKET-NUMBER. If

Returns nil, if Jira CLI fails for any reason: auth error,
non-existent ticket, etc."
  (->> ticket-number
      jira--get-ticket-details
      (alist-get 'fields)
      (alist-get 'summary)))

;; https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
(defun org-kill-link ()
  (interactive)
  "Deletes whole link, if the thing-at-point is a proper Org-link"
  (when (org-in-regexp org-bracket-link-regexp 1)
    (let ((remove (list (match-beginning 0) (match-end 0)))
          (description (if (match-end 3)
                           (org-match-string-no-properties 3)
                         (org-match-string-no-properties 1))))
      (apply 'delete-region remove))))

(defun markdown-kill-link ()
  (interactive)
  (when (org-in-regexp markdown-regex-link-inline 1)
    (let ((remove (list (match-beginning 0) (match-end 0)))
          (description (if (match-end 3)
                           (org-match-string-no-properties 3)
                         (org-match-string-no-properties 1))))
      (apply 'delete-region remove))))

(defun jira--convert-number-to-ticket-key (ticket-number)
  (let* ((ticket (string-to-number (replace-regexp-in-string "[^0-9]" "" ticket-number)))
         (prefix (replace-regexp-in-string "[0-9]" "" ticket-number))
         (jira-project-prefix (if (string-empty-p prefix)
                                  jira-project
                                prefix)))
    (concat jira-project-prefix (number-to-string ticket))))

(defun convert-to-jira-link ()
  "Converts a word (simple number or a JIRA ticket with prefix) to a proper JIRA link."
  (interactive)
  (let* ((w (symbol-at-point))
         (bounds (bounds-of-thing-at-point 'symbol))
         (ticket (string-to-number (replace-regexp-in-string "[^0-9]" ""
                                                             (symbol-name w))))
         (prefix (replace-regexp-in-string "[0-9]" "" (symbol-name w)))
         (jira-project-prefix (if (string-empty-p prefix)
                                  jira-project
                                prefix))
         (uri (concat
               jira-base-url
               "/browse/"
               (concat jira-project-prefix (number-to-string ticket))))
         (label (string-trim
                 (if (string-match-p jira-project-prefix (downcase (symbol-name w)))
                     (upcase (symbol-name w))
                   (upcase (concat jira-project-prefix (number-to-string ticket))))))
         (summary (jira--get-ticket-summary label))
         (summary-lbl (when summary (concat ": " summary))))

    (cond ((eq major-mode 'org-mode)
           (progn
             (if (org-in-regexp org-bracket-link-regexp 1)
                 (org-kill-link)
               (delete-region (car bounds) (cdr bounds)))
             (insert (concat "[[" uri "][" label summary-lbl "]]"))))

          ((eq major-mode 'markdown-mode)
           (progn
             (if (org-in-regexp markdown-regex-link-inline 1)
                 (markdown-kill-link)
               (delete-region (car bounds) (cdr bounds)))
             (insert (concat "[" label summary-lbl "](" uri ")"))))

          ((string-match-p "COMMIT_EDITMSG" (or buffer-file-name ""))
           (progn
             (delete-region (car bounds) (cdr bounds))
             (insert (concat "[" label summary-lbl "]\n" uri))))

          (t
           (progn
             (delete-region (car bounds) (cdr bounds))
             (insert (concat uri "\n" summary)))))))

(defun jira-generate-git-branch-name (&optional ticket-number)
  "Generates readable branch name base on given TICKET-NUMBER"
  (interactive)
  (let* ((w (symbol-name (symbol-at-point)))
         (ticket-key (if (s-blank? (replace-regexp-in-string "[^0-9]" "" w))
                         (jira--convert-number-to-ticket-key (read-string "Enter Jira ticket number: "))
                       (jira--convert-number-to-ticket-key w)))

         (summary (jira--get-ticket-summary ticket-key))
         (norm-summary
          (-some->>
           summary
           downcase
           (replace-regexp-in-string " \\|\\-" "_" )
           (replace-regexp-in-string
            "\\.\\|\\,\\|\\#\\|\\?\\|\\!\\|\\*\\|\\\\\\|\\/\\|\\:\\|\\~\\|\\^\\|\\[\\|\\]\\|\\@\\|" "")))
         (branch-name (concat norm-summary "--" ticket-key)))
    (kill-new branch-name)
    (message branch-name)
    branch-name))

(provide 'jira)
