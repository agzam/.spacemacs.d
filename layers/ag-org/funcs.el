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

  ;; (variable-pitch-mode)
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
(make-obsolete 'org-roam--org-heading->note "function breaks in Org-roam v2." "2021-07-21")

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

(defun org-store-link-id-optional (&optional arg)
  "Stores a link, reversing the value of `org-id-link-to-org-use-id'.
If it's globally set to create the ID property, then it wouldn't,
and if it is set to nil, then it would forcefully create the ID."
  (interactive "P")
  (let ((org-id-link-to-org-use-id (not org-id-link-to-org-use-id)))
    (org-store-link arg :interactive)))

(defun org-roam-title-convert ()
  "Converts drawer based title in org-roam zettel file"
  (interactive)
  (save-mark-and-excursion
    (goto-char 0)
    (when-let ((title (car (org--property-local-values "title" nil))))
      (unless (search-forward "#+title" nil :noerror)
        (goto-char 0)
        (org-delete-property "title")
        (search-forward ":END:")
        (insert "\n")
        (insert (concat "#+title: " title))))))

(defun org-attach-save-file-list-to-property (dir)
  "Save list of attachments to ORG_ATTACH_FILES property."
  (when-let* ((files (org-attach-file-list dir)))
    (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", "))))

(defun outline-collapsed? ()
  "Returns nil if the top outline heading is collapsed (hidden)"
  (save-excursion
    (when (org-get-outline-path 'with-self?)
      (ignore-errors (outline-up-heading 1))
      (let* ((beg (point))
             (end (+ 1 (line-end-position))))
        (not
         (seq-empty-p
          (seq-filter
           (lambda (o)
             (and (eq (overlay-get o 'invisible) 'outline)
                  (save-excursion
                    (goto-char (overlay-start o))
                    (outline-on-heading-p t))))
           (overlays-in beg end))))))))

(defun org--get-headline-with-text ()
  "Grabs top level headline and its content"
  (save-excursion
    (save-restriction
      (ignore-errors (outline-up-heading 1))
      (let ((heading-shown? (not (outline-collapsed?))))
        (when heading-shown? (hide-subtree))
        (let* ((elt (org-element-at-point))
               (title (org-element-property :title elt))
               (beg (progn (org-end-of-meta-data t) (point)))
               (end (progn (outline-next-visible-heading 1) (point))))
          (when heading-shown? (show-subtree))
          (list title (buffer-substring-no-properties beg end)))))))

(defun org-roam-refile-to-node ()
  "Refile heading to another Org-roam node."
  (interactive)
  (let* ((headline? (org-get-outline-path 'with-self?))
         (props (org-entry-properties))
         (id (org-entry-get nil "id"))
         (title (if headline?
                    (org-element-property :title (org-element-at-point))
                  (cadar (org-collect-keywords '("title")))))
         (content (cadr (org--get-headline-with-text))))
    (when headline?
      (org-cut-subtree))
    (org-roam-capture-
     :goto nil
     :node (org-roam-node-create
            :title title
            :properties props
            :id id)
     :props '(:immediate-finish nil))
    (let ((insert-at (plist-get org-capture-plist :position-for-last-stored)))
      (with-current-buffer (marker-buffer insert-at)
        (insert content)
        (mark-whole-buffer)
        (org-do-promote)))))

;; filetag helpers borrowed from:
;; https://magnus.therning.org/2021-07-23-keeping-todo-items-in-org-roam-v2.html

(defun org-roam/get-filetags ()
  (split-string (or (org-roam-get-keyword "filetags") "")))

(defun org-roam/add-filetag (tag)
  (let* ((new-tags (cons tag (org-roam/get-filetags)))
         (new-tags-str (combine-and-quote-strings new-tags)))
    (org-roam-set-keyword "filetags" new-tags-str)))

(defun org-roam/kill-filetag (tag)
  (let* ((new-tags (seq-difference (org-roam/get-filetags) `(,tag)))
         (new-tags-str (combine-and-quote-strings new-tags)))
    (org-roam-set-keyword "filetags" new-tags-str)))

(defun org-roam/org-todo-keywords->list ()
  (let ((lst (-partition-by
              (lambda (s) (string-equal s "|"))
              (seq-map
               (lambda (s) (replace-regexp-in-string "\(.*" "" s))
               (cl-rest (car org-todo-keywords))))))
    `(,(car lst) ,(caddr lst))))

(defun org-roam/node-find-todos (todo-state)
  "Finds all nodes with selected TODO state.
   For headings that would be regular Org-mode TODO cookie,
   for file notes it's managed with TODO filetag."
  (interactive "P")
  (let ((todo-state (or todo-state
                        (completing-read
                         "Choose TODO state"
                         (-flatten (org-roam/org-todo-keywords->list))))))
    (org-roam-node-find
     :other-window nil
     (lambda (node)
       (or (string-equal todo-state (org-roam-node-todo node))
           (seq-contains-p (org-roam-node-tags node) todo-state)
           (seq-contains-p (org-roam-node-tags node) todo-state))))))

(defun org-roam/toggle-todo-state-in-node (&optional new-state)
  "Togggle TODO filetag in Org-roam node file."
  (interactive "P")
  (when (and (not (active-minibuffer-window))
             (org-roam-file-p))
    (let* ((all-states (-flatten (org-roam/org-todo-keywords->list)))
           (new-state (or new-state
                          (completing-read "Choose TODO state" all-states)))
           (current (seq-find
                     (lambda (tag)
                       (seq-contains-p all-states tag))
                     (org-roam/get-filetags))))
      (when current (org-roam/kill-filetag current))
      (when (not (string-equal current new-state))
        (org-roam/add-filetag new-state)))))

(defun org-todo--around (old-fn &rest args)
  (if (and (not (active-minibuffer-window))
           (org-roam-file-p)
           (not (ignore-errors (org-get-outline-path 'with-self?))))
      (funcall 'org-roam/toggle-todo-state-in-node)
    (apply old-fn args)))

(advice-add 'org-todo :around #'org-todo--around)

(defun org-roam--link-to (node-title-or-id)
  "For a given NODE-TITLE-OR-ID tries to find the node and returns
Org-link text to the node."
  (when-let* ((nodes (org-roam-db-query
                      "select n.id, n.title from nodes n
                       left join aliases a
                       on n.id = a.node_id
                       where n.id = $s1
                         or title = $s1 collate nocase
                         or a.alias = $s1 collate nocase"
                      node-title-or-id))
              (node (cl-first nodes)))
    (apply 'format "[[id:%s][%s]]" node)))

(defun org-roam-capture--add-link-to (node-title-or-id)
  (with-current-buffer (org-capture-get :buffer)
   (save-mark-and-excursion
     (let ((lnk (org-roam--link-to node-title-or-id)))
       (goto-char 0)
       (unless (search-forward lnk nil :no-error)
         lnk)))))

(cl-defun org-roam-node-insert+ (&optional lines-before lines-after &key templates info)
  "Improved org-roam-node-insert that additionally also removes conflicting and
duplicating links around the context.  If a node has 'collides_with:' property,
inserting a link to that node would remove any links to nodes with IDs contained
in that prop."
  (interactive)
  (unwind-protect
      (atomic-change-group
        (let* (region-text
               beg end
               (_ (when (region-active-p)
                    (setq beg (set-marker (make-marker) (region-beginning)))
                    (setq end (set-marker (make-marker) (region-end)))
                    (setq region-text (org-link-display-format (buffer-substring-no-properties beg end)))))
               (node-to-insert (org-roam-node-read region-text))   ; first we choose a node to insert
               (description (or region-text (org-roam-node-title node-to-insert)))
               ;; then we find the IDs of nodes, links that need to be removed if that link to be inserted
               (id+collides (seq-remove
                             'string-empty-p
                             (-> node-to-insert
                                 (org-roam-node-properties)
                                 (a-get "COLLIDES_WITH")
                                 (concat " " (org-roam-node-id node-to-insert))
                                 (split-string " "))))
               (re (concat "\\(\\[\\[\\)\\(id:\\|roam:\\)"
                           "\\(" (mapconcat 'identity id+collides "\\|")
                           "\\)\\]\\[\\w*\\]\\]"))
               (before (or lines-before 0))
               (after (or lines-after 0)))
          (save-mark-and-excursion
            (unless region-text
             (previous-line before)
             (beginning-of-line)
             (set-mark (point))
             (next-line (+ before after))
             (end-of-line))
            (save-restriction
              (narrow-to-region (region-beginning) (region-end))
              (if (org-roam-node-id node-to-insert)
                  (progn
                    (when region-text
                      (delete-region beg end)
                      (set-marker beg nil)
                      (set-marker end nil))
                    (unless region-text
                      (goto-char 0)
                      (while (re-search-forward re nil :no-error)
                        (replace-match "")))
                    (insert (org-link-make-string
                             (concat "id:" (org-roam-node-id node-to-insert))
                             description)))
                (org-roam-capture-
                 :node node-to-insert
                 :info info
                 :templates templates
                 :props (append
                         (when (and beg end)
                           (list :region (cons beg end)))
                         (list :insert-at (point-marker)
                               :link-description description
                               :finalize 'insert-link
                               :immediate-finish t
                               :jump-to-captured nil))))
              ;; make sure links always separated by a single space
              (goto-char 0)
              (while (re-search-forward "\\]\\]\\[\\[" nil :no-error)
                (replace-match "]] [["))))))))

(provide 'funcs)

;;; funcs.el ends here
