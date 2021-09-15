;;; funcs.el --- ag-notmuch layer functions
;;
;; Copyright (c) 2017 Ag Ibragimov
;;
;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Code:

(defun spacemacs//notmuch-inbox-p (saved-search-property-item)
  "Returns non-nil if item is the inbox."
  (string-equal (plist-get saved-search-property-item :name) "inbox"))

(defun spacemacs/notmuch-inbox ()
  "Search inbox."
  (interactive)
  (notmuch-search
   (plist-get (nth 0 (-filter 'spacemacs//notmuch-inbox-p notmuch-saved-searches))
              :query)))

(defun spacemacs/notmuch-search-archive-thread-down ()
  "Search thread up."
  (interactive)
  (notmuch-search-archive-thread))

(defun spacemacs/notmuch-search-archive-thread-up ()
  "Search thread down."
  (interactive)
  (notmuch-search-archive-thread)
  (notmuch-search-previous-thread)
  (notmuch-search-previous-thread))

(defun spacemacs//notmuch-tree-message-delete (go-next)
  "Delete message and select GO-NEXT message."
  (notmuch-tree-tag notmuch-message-deleted-tags)
  (if (eq 'up go-next)
      (notmuch-tree-prev-thread)
    (notmuch-tree-next-thread)))

(defun spacemacs/notmuch-tree-message-delete-down ()
  "Delete a message and select the next message."
  (interactive)
  (spacemacs//notmuch-tree-message-delete 'down))

(defun spacemacs/notmuch-tree-message-delete-up ()
  "Delete a message and select the previous message."
  (interactive)
  (spacemacs//notmuch-tree-message-delete 'up))

(defun spacemacs//notmuch-search-message-delete (go-next)
  "Delete message and select GO-NEXT message."
  (notmuch-search-tag notmuch-message-deleted-tags)
  (if (eq 'up go-next)
      (notmuch-search-previous-thread)
    (notmuch-search-next-thread)))

(defun spacemacs/notmuch-search-message-delete-down ()
  "Delete a message and select the next message."
  (interactive)
  (spacemacs//notmuch-search-message-delete 'down))

(defun spacemacs/notmuch-search-message-delete-up ()
  "Delete a message and select the previous message."
  (interactive)
  (spacemacs//notmuch-search-message-delete 'up))

(defun spacemacs/notmuch-show-as-patch ()
  (interactive)
  (notmuch-show-choose-mime-of-part "text/x-patch"))

(defun spacemacs/notmuch-show-close-all ()
  "Close all."
  (interactive)
  (goto-char (point-min))
  (let ((current-prefix-arg '(4)))
    (call-interactively 'notmuch-show-open-or-close-all)))


;; git

(defun spacemacs/notmuch-git-apply-patch (entire-thread)
  "Apply patch from a notmuch-show email buffer to a git repository

If ENTIRE-THREAD is non-nil it will apply patches from all open
messages in the current thread"
  (interactive "P")
  (notmuch-show-pipe-message entire-thread "git am"))

(defun spacemacs/notmuch-git-apply-patch-part ()
  "Apply patch attached to a message as MIME part to a git repository."
  (interactive)
  (let ((mime-type nil))
    (notmuch-show-apply-to-current-part-handle
     (lambda ()
       (mm-pipe-part (notmuch-show-current-part-handle mime-type) "git am")))))


;; GitHub

;; Thanks to Kyle Meyer (@kyleam)
(defun spacemacs//notmuch-open-github-patch (buffer)
  "Find GitHub patch link in BUFFER and show it in a new buffer."
  (let ((url
         (with-current-buffer buffer
           (save-excursion
             (goto-char (point-min))
             (if (re-search-forward "https://github.com/.*\\.patch" nil t)
                 (match-string-no-properties 0)
               (if (re-search-forward "https://github.com/[^/]+/[^/]+/pull/[0-9]+" nil t)
                   (concat (match-string-no-properties 0) ".patch")
                 (user-error "No patch found")))))))
    (with-current-buffer (get-buffer-create
                          (generate-new-buffer-name "*mail-github-patch*"))
      (condition-case exception
          (url-insert-file-contents url)
        ('file-error
         ;; In case the link is private repository github will respond with a
         ;; temporary redirect 302 HTTP code and calculate the request-token
         ;; with javascript. In this case open diff in browser
         (browse-url url)))
      (diff-mode)
      (view-mode 1)
      (pop-to-buffer-same-window (current-buffer)))))

(defun spacemacs/notmuch-show-open-github-patch ()
  "Open patch from GitHub email."
  (interactive)
  (with-current-notmuch-show-message
   (spacemacs//notmuch-open-github-patch (current-buffer))))


;; persp

(defun spacemacs//notmuch-persp-filter-save-buffers-function (buffer)
  "Filter for notmuch layout."
  (with-current-buffer buffer
    (memq major-mode notmuch-modes)))

(defun spacemacs//notmuch-buffer-to-persp ()
  "Add buffer to notmuch layout."
  (persp-add-buffer (current-buffer)
                    (persp-get-by-name notmuch-spacemacs-layout-name)))

(defun notmuch-open-in-gmail ()
  "Open message in Gmail Web App"
  (interactive)
  (let* ((msg-id (notmuch-show-get-message-id :bare))
         (url (concat
               "https://mail.google.com/mail/u/0/#search/rfc822msgid"
               (url-hexify-string (concat ":" msg-id)))))
    (print url)
    (when url
      (message "opening url: " url)
      (browse-url url))))

(defun notmuch-find-in-mailing-list ()
    "Find message in mailing-list archives"
    (interactive)
    (let* ((mailing-groups '("gnu.org" "googlegroups.com"))
           (headers (plist-get (notmuch-show-get-message-properties) :headers))
           (send-to (concat (plist-get headers :To) ", " (plist-get headers :Cc)))
           (msg-id (notmuch-show-get-message-id :bare))

           ;; figure out the mailing-group index by finding first matching
           ;; address in send-to field
           (mlist (cl-some
                   (lambda (x)
                     (when (string-match (concat "\\([[:graph:]]*\\)@" x) send-to)
                       ;; email addresses often contain < and >, e.g.: Vasya Pupkin <vasya@mail.ru>
                       `(,(replace-regexp-in-string "<\\|>" "" (match-string 1 send-to)) ,x)))
                   mailing-groups))

           (url
            (pcase mlist
              ;; gnu.org
              ;; for some reason it's now broken. It looks like
              ;; something has changed in the portal
              ((pred (lambda (x) (string-match-p "gnu.org" (cadr x))))
               (concat
                "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
                (concat
                 (url-hexify-string
                  (concat
                   "+message-id:<"
                   msg-id
                   ">"))
                 "&submit=" (url-hexify-string "Search!")
                 "&idxname="
                 (car mlist))))

              ;; google.groups
              ((pred (lambda (x) (string-match-p "googlegroups.com" (cadr x))))
               (concat
                "https://groups.google.com/forum/#!topicsearchin/"
                (car mlist)
                "/messageid$3A"
                (url-hexify-string (concat "\"" msg-id "\"")))))))
      (when url
        (message "opening url: " url)
        (browse-url url))))

;;; funcs.el ends here
