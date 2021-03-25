;;; funcs.el --- Org-mode functions
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

(defun insert-current-date (arg)
  "Insert today's date.
Without universal ARG: 2017-11-08 With universal ARG: 08.11.2017"
  (interactive "P")
  (insert (if arg
              (format-time-string "%d.%m.%Y")
            (format-time-string "%Y-%m-%d"))))

(defun org-refile-to-datetree (&optional file)
  "Refile a subtree to a datetree corresponding to it's timestamp.

Will prompt for timestamp if the entry has no timestamp.
If FILE is nil, refile in the current buffer."
  (interactive)
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil)))
         (date (org-date-to-gregorian datetree-date)))
    (save-excursion
      (with-current-buffer (current-buffer)
        (org-cut-subtree)
        (when file
          (find-file file))
        (org-datetree-find-date-create date)
        (org-narrow-to-subtree)
        (show-subtree)
        (org-end-of-subtree t)
        (goto-char (point-max))
        (org-paste-subtree 4)
        (widen)))))

(defun org-goto-datetree-date (&optional date)
  (interactive)
  (save-restriction
    (let* ((datetree-date (org-read-date))
           (dt (org-date-to-gregorian datetree-date)))
      (org-datetree-find-date-create dt t)
      (org-show-hidden-entry)
      (show-subtree))))

(defun org-goto-last-heading (&optional maxlevel)
  "Go to the last heading in the current subtree."
  (interactive "P")
  (if (listp maxlevel)
      (setq maxlevel 4)
    (unless maxlevel (setq maxlevel 3)))
  (setq currlevel 1)
  (while (<= currlevel maxlevel)
    (org-next-visible-heading 1)
    (if (not (org-at-heading-p))
        (progn
          (org-previous-visible-heading 1)
          (org-cycle)
          (setq currlevel (1+ currlevel))))))

(defun pomodoro/create-menu-item (color)
  "Create Hammerspoon `hs.menubar`.

item COLOR can be \"red\" \"green\" or \"yellow\"."
  (let* ((hs (executable-find "hs"))
         (task-name (symbol-value 'org-clock-current-task))
         (cmd (concat "if globalMenubarItem then globalMenubarItem:delete() end; "
                      "txt = hs.styledtext.new(\""
                      task-name
                      "\",{ color = hs.drawing.color.hammerspoon.osx_" color " });"
                      "globalMenubarItem = hs.menubar.newWithPriority(0);"
                      "globalMenubarItem:setTitle(txt)")))
    (call-process hs
                  nil 0 nil
                  "-c" cmd)))

(defun pomodoro/modify-menu-item (color)
  "Change COLOR of the OSX menu item, previously created by `pomodoro/create-menu-item'."
  (let* ((hs (executable-find "hs"))
         (cmd (concat "if globalMenubarItem then "
                      "txt = hs.styledtext.new(globalMenubarItem:title() "
                      ",{ color = hs.drawing.color.hammerspoon.osx_" color " });"
                      "globalMenubarItem:setTitle(txt);"
                      "end")))
    (message cmd)
    (call-process hs
                  nil 0 nil
                  "-c" cmd)))

(defun pomodoro/remove-menu-item ()
  "Remove previously set pomodoro item - `hs.menubar` item."
  (let* ((hs (executable-find "hs"))
         (cmd " globalMenubarItem:delete(); globalMenubarItem = nil"))
    (call-process hs
                  nil 0 nil
                  "-c" cmd)))

(defun pomodoro/on-finished-hook ()
  "When pomodoro is done."
  (cond
   ((eq system-type 'darwin)
    (progn
      (spacehammer/alert "task done")
      (pomodoro/modify-menu-item "green")))
   ((eq system-type 'gnu/linux)
    (notify-send "org-pomodoro" "Task is done, good job!" '(category "finished")))))

(defun pomodoro/on-break-over-hook ()
  "When pomodoro break is over."
  (cond ((eq system-type 'darwin)
         (progn (spacehammer/alert "break over")
                (pomodoro/remove-menu-item)))
        ((eq system-type 'gnu/linux)
         (notify-send "org-pomodoro" "Break is over, get back to work"
                      '(category "break-over")))))

(defun pomodoro/on-killed-hook ()
  "When you kill pomodoro."
  (cond ((eq system-type 'darwin)
         (progn (spacehammer/alert "killed")
                (pomodoro/remove-menu-item)))
        ((eq system-type 'gnu/linux)
         (notify-send "org-pomodoro" "Pomodoro got killed"
                      '(category "killed")))))

(defun pomodoro/on-started-hook ()
  "When pomodoro starts."
  (cond
   ((eq system-type 'darwin)
    (progn
      (spacehammer/alert "- start churning -")
      (pomodoro/create-menu-item "red")))
   ((eq system-type 'gnu/linux)
    (notify-send "org-pomodoro" "Let's get to it" '(category "started")))))

(defun ag/org-mode-hook ()
  ;; completion on Tab for `#+` stuff
  ;; (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)

  (variable-pitch-mode)
  ;; fix indentation for variable pitch
  (require 'org-indent)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  )

(defun ag/add-days-to-ifttt-date (datetime days)
  "Takes DATETIME in ifttt.com format e.g. `February 23, 2017 at 11:00AM` and turns it into emacs-lisp datetime and then adds given number of DAYS."
  (-some-> datetime
           (substring 0 -2)
           (split-string  " " nil ",")
           ((lambda (x) (cons (car (cdr x)) (cons (car x) (cdr (cdr x))))))
           ((lambda (x) (mapconcat 'identity x " ")))
           (date-to-time)
           (time-add (days-to-time days))
           ((lambda (x) (format-time-string "%Y-%m-%d" x)))))

(defun ag/indent-org-entry ()
  "Properly sets indentation for the current org entry."
  (interactive)
  (outline-show-entry)
  (forward-line 1)
  (set-mark-command nil)
  (org-end-of-subtree)
  (backward-char)
  (indent-region (region-beginning) (region-end) 2)
  (flush-lines "^$" nil nil t)
  (outline-hide-entry))

(defun ag/org-toggle-tags (tags)
  "Toggle TAGS on the current heading."
  (let ((existing (org-get-tags-at nil t)))
    (when tags
      (dolist (i tags)
        (when (not (member i existing))
          (org-toggle-tag i 'on))))))

(defun ag/org-url? ()
  "If org heading title is an org-link, it check if the link is 'broken' - contains line-breaks, then it fixes it."
  (let* ((title (org-entry-get (point) "ITEM"))
         (url? (string-match "\\[\\[.*" title))
         (proper-url? (string-match "\\[\\[.*\\]\\[.*\\]\]" title)))
    url?))

(defun ag/org-fix-heading-line-break (&optional pnt)
  "If org heading title is an org-link, it check if the link is 'broken' - contains line-breaks, then it fixes it."
  (let* ((title (org-entry-get (or pnt (point)) "ITEM"))
         (url? (string-match "\\[\\[.*" title))
         (proper-url? (string-match "\\[\\[.*\\]\\[.*\\]\]" title)))
    (if (and url? (not proper-url?))
        (if (re-search-forward "\n")
            (progn
              (replace-match " ")
              (ag/org-fix-heading-line-break))))))

(defun ag/--process-ifttt-pocket-entry ()
  "Process individually a single entry in org 'pocket'"
  (ag/org-fix-heading-line-break (point))
  (ag/indent-org-entry)
  (let* ((start (org-entry-beginning-position)))
    (let ((tags (-some-> (org-entry-get start "tag")
                         (split-string "," t "\s")))
          (deadline (org-entry-get start "DEADLINE"))
          (added-at (org-entry-get start "AddedAt")))
      ;; set the tags of the heading based on 'Tag' property
      (ag/org-toggle-tags tags)
      (when (and added-at (not deadline))
        (org--deadline-or-schedule nil 'deadline (ag/add-days-to-ifttt-date added-at 30)))
      (append-to-file (org-entry-beginning-position) (org-entry-end-position) "~/Dropbox/org/read-later.org")
      (delete-region (org-entry-beginning-position) (org-entry-end-position)))))

(defun ag/process-ifttt-pocket ()
  "Process org 'pocket' - entries imported via IFTTT"
  (let ((buf (find-file-noselect "~/Dropbox/org/pocket.txt")))
    (set-buffer buf)
    (org-map-entries (lambda ()
                       (ag/--process-ifttt-pocket-entry)
                       (setq org-map-continue-from (outline-previous-heading))) nil 'file)
    ;;; for whatever stupid reason, it would not process the last entry, I couldn't figure out how to fix that, so ended up scanning the list twice
    (org-map-entries 'ag/--process-ifttt-pocket-entry nil 'file)
    (save-buffer)
    (kill-buffer)))

(defun convert-markdown-link-to-org ()
  (interactive)
  (when (markdown-link-p)
    (let* ((l (markdown-link-at-pos (point)))
           (desc (nth 2 l))
           (url (nth 3 l)))
      (markdown-kill-thing-at-point)
      (org-insert-link nil url desc))))

(defun get-gh-item-title (uri)
  "Based on given GitHub URI for pull-request or issue,
  return the title of that pull-request or issue."
  (cond ((string-match "\\(github.com\\).*\\(issues\\|pull\\)" uri)               ; either PR or issue
         (pcase-let* ((`(_ _ ,owner ,repo ,type ,number) (remove "" (split-string uri "/")))
                      (gh-resource (format "/repos/%s/%s/%s/%s"
                                           owner
                                           repo
                                           (if (string= type "pull") "pulls" type)
                                           number))
                      (resp (ghub-get gh-resource nil :auth 'forge)))
           (when resp
             (let-alist resp
               (format
                "%s/%s#%s %s" owner repo number .title)))))

        ((string-match "\\(github.com\\).*" uri)          ; just a link to a repo or file in a branch
         (pcase-let* ((uri*  (->> (split-string uri "/\\|\\?")
                                  (remove "")
                                  (-non-nil)))
                      (`(_ _ ,owner ,repo ,type ,branch ,dir ,file) uri*)
                      (branch (if (or (string= type "commit") (string= type "tree"))
                                (substring branch 0 7)        ; trim to short sha
                              branch)))
           (mapconcat
            'identity (->> (list owner repo type branch dir file) (-non-nil))
            "/")))

        (t uri)))

(defun org-link-make-description-function* (link desc)
  (cond ((not (s-blank? desc)) desc)
        ((string-match "\\(github.com\\).*" link)
         (get-gh-item-title link))
        (t desc)))

(defun org-return--around (old-fn &rest args)
  "Smarter org-return in lists. RET in plain lists would insert
items, or checkboxes - depending on the current context. C-j
splits the line."
  (let ((context (org-element-lineage (org-element-at-point) '(item))))
    (if (and context (< 1 (safe-length args)))
        (org-insert-item (org-element-property :checkbox context))
      (apply old-fn args))))

(defun org-roam--set-last-modified ()
  "Update the LAST_MODIFIED file property of org-roam note."
  (when (and buffer-file-name
             (derived-mode-p 'org-mode)
             ;; buffer dir is a subdir of the org-roam dir
             (string-prefix-p
              (expand-file-name org-roam-directory)
              (expand-file-name (file-name-directory buffer-file-name))))
    (save-mark-and-excursion
      (goto-char 0)
      (org-set-property "last_modified" (format-time-string "[%Y-%m-%d %a %H:%M:%S]")))))

;; Borrowed from https://org-roam.discourse.group/t/creating-an-org-roam-note-from-an-existing-headline
(defun org-roam-create-note-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-find-file title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))))

(defun zp/org-protocol-insert-selection-dwim (selection)
  "Insert SELECTION as an org blockquote."
  (unless (string= selection "")
    (format "#+begin_quote\n%s\n#+end_quote" selection)))

(defun looking-at-org-link? ()
  "Return true if cursor is at org-mode link."
  (save-mark-and-excursion
    (let ((end-re "\]\]"))
      (when (looking-at (concat "\\([^[:blank:]]\\)*" end-re))
        (search-forward-regexp end-re nil :no-error)
        (search-backward-regexp "\\[\\[.*" nil :no-error)
        (set-mark (point))
        (search-forward-regexp end-re nil :no-error)
        (exchange-point-and-mark)
        (equal 0
               (string-match
                "\\[\\[.*\\]\\[.*\\]\\]"
                (buffer-substring (region-beginning) (region-end))))))))

(defun www-get-page-title (url)
  "Attempts to retrive document.title for a given URL"
  (let ((temp-buffer-show-function (lambda (x))))
    (with-output-to-temp-buffer url
      (with-current-buffer url
        (url-insert-file-contents url t)
        (goto-char (point-min))
        (when (re-search-forward "<title>\\([^<]*\\)</title>" nil t 1)
          (match-string 1))
        (setq title (match-string 1))))
    (kill-buffer url)
    title))

(defun org-roam--org-heading->note ()
  "Reads a org heading and captures new note using org-roam-ref template."
  (interactive)
  (require 'expand-region)
  (require 'org-agenda)
  (pcase-let* ((url-regexp "\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)")
               (`(,l ,rl ,todo-cookie ,priority ,headline ,tags) (org-heading-components))
               (tags (when tags (split-string tags ":" t)))
               (props (org-entry-properties))
               (url (alist-get "URL" props nil nil 'string-match))
               (added-at (alist-get "ADDEDAT" props nil nil 'string-match))
               (content (org-agenda-get-some-entry-text (point-marker) most-positive-fixnum))
               (buf (concat "*org-heading->note *" headline))
               (roam-links (apply 'concat (mapcar (lambda (x) (format " [[roam:%s]]" x)) tags)))
               (more-links? t)
               ;; discard temp buffer after manipulations
               (temp-buffer-show-function (lambda (x) (kill-buffer x))))
    ;; using a temp buffer for finding and replacing all url with org-links
    (with-output-to-temp-buffer buf
      (with-current-buffer buf
        (insert (format "%s \n\n* %s %s\n:PROPERTIES:\n:AddedAt: %s\n:END:\n%s"
                        roam-links
                        todo-cookie
                        headline
                        added-at
                        content))

        ;; convert urls to org-links
        (goto-char 0)
        (while more-links?
          (setq more-links? (re-search-forward url-regexp nil t))
          (when (and more-links? (not (looking-at-org-link?)))
            (er/mark-url)
            (if-let* ((url (buffer-substring (region-beginning) (region-end)))
                      (title (www-get-page-title url)))
                (progn
                  (kill-region (region-beginning) (region-end))
                  (insert (concat "[[" url "][" title "]]")))
                (forward-word))))

        ;; pick up any first url, if there's no url in the properties drawer
        (goto-char 0)
        (unless url
          (re-search-forward url-regexp nil t)
          (er/mark-url)
          (setq url (buffer-substring (region-beginning) (region-end))))

        (setq body (buffer-string))))

    (let* ((uri (org-protocol-sanitize-uri url))
           (type (and (string-match "^\\([a-z]+\\):" uri) (match-string 1 uri)))
           (capture-info `((ref . ,uri)
                           (type . ,type)
                           (title . ,headline)
                           (slug . ,(funcall org-roam-title-to-slug-function headline))
                           (body . ,body)
                           (orglink . ,(org-link-make-string uri))))
           (org-roam-capture-templates org-roam-capture-ref-templates)
           (org-roam-capture--context 'ref)
           (org-roam-capture--info capture-info)
           (org-capture-link-is-already-stored t)
           (template (cdr (assoc 'template capture-info))))
      (org-roam-capture--capture nil "p")
      (org-roam-message "Item captured."))))

(defun org-roam-keywords->prop-drawer ()
  "Converts file level Roam key/value pairs by putting them in a
  prop drawer."
  (interactive)
  (save-excursion
    (goto-char 0)
    (let* ((existing (org-roam--extract-global-props-keyword '("title" "created" "last_modified" "roam_alias" "roam_key" "roam_tags")))
           (title (alist-get "TITLE" existing nil nil 'string-match)))
      (dolist (kv existing)
        (org-set-property (downcase (car kv)) (s-trim (cdr kv)))
        (flush-lines (concat "#\\+" (downcase (car kv)))))
      (re-search-forward ":END:\n")
      (when title
        (insert (format "* %s\n" title))))))

(defun org-roam--reset-roam-buffer-window (&optional win)
  (run-at-time
   "0.01" nil
   (lambda ()
    (pcase (org-roam-buffer--visibility)
      ('visible (org-roam-buffer-activate))))))

(provide 'funcs)

;;; funcs.el ends here
