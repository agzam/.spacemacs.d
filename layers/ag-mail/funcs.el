;;; funcs.el --- ag-mail layer functions
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

(defun mu4e-prepare-view ()
  (run-at-time
   "0.1 sec" nil
   (lambda ()
     (setq writeroom-fullscreen-effect (frame-parameter (selected-frame) 'fullscreen))
     (spacemacs/toggle-centered-buffer))))

(with-eval-after-load 'hydra
  (defhydra hydra-mu4e-headers (:color blue :hint nil)
  "
 ^General^   | ^Search^           | _!_: read    | _#_: deferred  | ^Switches^
-^^----------+-^^-----------------| _?_: unread  | _%_: pattern   |-^^------------------
_n_: next    | _s_: search        | _r_: refile  | _&_: custom    | _O_: sorting
_p_: prev    | _S_: edit prev qry | _u_: unmk    | _+_: flag      | _P_: threading
_]_: n unred | _/_: narrow search | _U_: unmk *  | _-_: unflag    | _Q_: full-search
_[_: p unred | _b_: search bkmk   | _d_: trash   | _T_: thr       | _V_: skip dups
_y_: sw view | _B_: edit bkmk     | _D_: delete  | _t_: subthr    | _W_: include-related
_R_: reply   | _{_: previous qry  | _m_: move    |-^^-------------+-^^------------------
_C_: compose | _}_: next query    | _a_: action  | _|_: thru shl  | _`_: update, reindex
_F_: forward | _C-+_: show more   | _A_: mk4actn | _H_: help      | _;_: context-switch
_o_: org-cap | _C--_: show less   | _*_: *thing  | _q_: quit hdrs | _j_: jump2maildir "

  ;; general
  ("n" mu4e-headers-next)
  ("p" mu4e-headers-previous)
  ("[" mu4e-select-next-unread)
  ("]" mu4e-select-previous-unread)
  ("y" mu4e-select-other-view)
  ("R" mu4e-compose-reply)
  ("C" mu4e-compose-new)
  ("F" mu4e-compose-forward)
  ("o" my/org-capture-mu4e)                  ; differs from built-in

  ;; search
  ("s" mu4e-headers-search)
  ("S" mu4e-headers-search-edit)
  ("/" mu4e-headers-search-narrow)
  ("b" mu4e-headers-search-bookmark)
  ("B" mu4e-headers-search-bookmark-edit)
  ("{" mu4e-headers-query-prev)              ; differs from built-in
  ("}" mu4e-headers-query-next)              ; differs from built-in
  ("C-+" mu4e-headers-split-view-grow)
  ("C--" mu4e-headers-split-view-shrink)

  ;; mark stuff
  ("!" mu4e-headers-mark-for-read)
  ("?" mu4e-headers-mark-for-unread)
  ("r" mu4e-headers-mark-for-refile)
  ("u" mu4e-headers-mark-for-unmark)
  ("U" mu4e-mark-unmark-all)
  ("d" mu4e-headers-mark-for-trash)
  ("D" mu4e-headers-mark-for-delete)
  ("m" mu4e-headers-mark-for-move)
  ("a" mu4e-headers-action)                  ; not really a mark per-se
  ("A" mu4e-headers-mark-for-action)         ; differs from built-in
  ("*" mu4e-headers-mark-for-something)

  ("#" mu4e-mark-resolve-deferred-marks)
  ("%" mu4e-headers-mark-pattern)
  ("&" mu4e-headers-mark-custom)
  ("+" mu4e-headers-mark-for-flag)
  ("-" mu4e-headers-mark-for-unflag)
  ("t" mu4e-headers-mark-subthread)
  ("T" mu4e-headers-mark-thread)

  ;; miscellany
  ("q" mu4e~headers-quit-buffer)
  ("H" mu4e-display-manual)
  ("|" mu4e-view-pipe)                       ; does not seem built-in any longer

  ;; switches
  ("O" mu4e-headers-change-sorting)
  ("P" mu4e-headers-toggle-threading)
  ("Q" mu4e-headers-toggle-full-search)
  ("V" mu4e-headers-toggle-skip-duplicates)
  ("W" mu4e-headers-toggle-include-related)

  ;; more miscellany
  ("`" mu4e-update-mail-and-index)           ; differs from built-in
  (";" mu4e-context-switch)
  ("j" mu4e~headers-jump-to-maildir)

  ("." nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu4e Thread Folding ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Refactored from https://github.com/djcb/mu/pull/783

(defun mu4e~headers-msg-unread-p (msg)
  "Check if MSG is unread."
  (let ((flags (mu4e-message-field msg :flags)))
    (and (member 'unread flags) (not (member 'trashed flags)))))

(defvar mu4e-headers-folding-slug-function
  (lambda (headers) (format " (%d)" (length headers)))
  "Function to call to generate the slug that will be appended to folded threads.
This function receives a single argument HEADERS, which is a list
of headers about to be folded.")

(defun mu4e~headers-folded-slug (headers)
  "Generate a string to append to the message line indicating the fold status.
HEADERS is a list with the messages being folded (including the root header)."
  (funcall mu4e-headers-folding-slug-function headers))

(defun mu4e~headers-fold-make-overlay (beg end headers)
  "Hides text between BEG and END using an overlay.
HEADERS is a list with the messages being folded (including the root header)."
  (let ((o (make-overlay beg end)))
    (overlay-put o 'mu4e-folded-thread t)
    (overlay-put o 'display (mu4e~headers-folded-slug headers))
    (overlay-put o 'evaporate t)
    (overlay-put o 'invisible t)))

(defun mu4e~headers-fold-find-overlay (loc)
  "Find and return the 'mu4e-folded-thread overlay at LOC, or return nil."
  (cl-dolist (o (overlays-in (1- loc) (1+ loc)))
    (when (overlay-get o 'mu4e-folded-thread)
      (cl-return o))))

(defun mu4e-headers-fold-all ()
  "Fold all the threads in the current view."
  (interactive)
  (let ((thread-id "") msgs fold-start fold-end)
    (mu4e-headers-for-each
     (lambda (msg)
       (end-of-line)
       (push msg msgs)
       (let ((this-thread-id (mu4e~headers-get-thread-info msg 'thread-id)))
         (if (string= thread-id this-thread-id)
             (setq fold-end (point))
           (when (< 1 (length msgs))
             (mu4e~headers-fold-make-overlay fold-start fold-end (nreverse msgs)))
           (setq fold-start (point)
                 fold-end (point)
                 msgs nil
                 thread-id this-thread-id)))))
    (when (< 1 (length msgs))
      (mu4e~headers-fold-make-overlay fold-start fold-end (nreverse msgs)))))

(defun mu4e-headers-toggle-thread-folding (&optional subthread)
  "Toggle the folding state for the thread at point.
If SUBTHREAD is non-nil, only fold the current subthread."
  ;; Folding is accomplished using an overlay that starts at the end
  ;; of the parent line and ends at the end of the last descendant
  ;; line. If there's no overlay, it means it isn't folded
  (interactive "P")
  (if-let ((o (mu4e~headers-fold-find-overlay (point-at-eol))))
      (delete-overlay o)
    (let* ((msg (mu4e-message-at-point))
           (thread-id (mu4e~headers-get-thread-info msg 'thread-id))
           (path-re (concat "^" (mu4e~headers-get-thread-info msg 'path)))
           msgs first-marked-point last-marked-point)
      (mu4e-headers-for-each
       (lambda (submsg)
         (when (and (string= thread-id (mu4e~headers-get-thread-info submsg 'thread-id))
                    (or (not subthread)
                        (string-match-p path-re (mu4e~headers-get-thread-info submsg 'path))))
           (push msg msgs)
           (setq last-marked-point (point-at-eol))
           (unless first-marked-point
             (setq first-marked-point last-marked-point)))))
      (when (< 1 (length msgs))
        (mu4e~headers-fold-make-overlay first-marked-point last-marked-point (nreverse msgs))))))

(defun mu4e-headers-thread-folded? ()
  (when (mu4e~headers-fold-find-overlay (point-at-eol)) t))

(defun mu4e-action-find-in-mailing-list (msg)
  "Find message in mailing-list archives"
  (interactive)
  (let* ((mlist (mu4e-message-field msg :mailing-list))
         (msg-id (mu4e-message-field msg :message-id))
         (url
          (pcase mlist

            ;; gnu.org
            ((pred (lambda (x) (string-suffix-p "gnu.org" x)))
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
               (replace-regexp-in-string "\.gnu\.org" "" mlist))))

            ;; google.groups
            ((pred (lambda (x) (string-suffix-p "googlegroups.com" x)))
             (concat
              "https://groups.google.com/forum/#!topicsearchin/"
              (replace-regexp-in-string "\.googlegroups\.com" "" mlist)
              "/messageid$3A"
              (url-hexify-string (concat "\"" msg-id "\"")))))))
    (when url
      (message "opening url: " url)
      (browse-url url))))

(defun mu4e-action-open-in-gmail (msg)
  "Open message in Gmail Web App"
  (interactive)
  (let* ((msg-id (mu4e-message-field msg :message-id))
         (url (concat
               "https://mail.google.com/mail/u/0/#search/rfc822msgid"
               (url-hexify-string (concat ":" msg-id)))))
    (print url)
    (when url
      (message "opening url: " url)
      (browse-url url))))

;; (browse-url "https://mail.google.com/mail/u/0/#search/rfc822msgid%3A0100016f7ebeffe1-d3290aa1-5df8-4b1b-b948-55f05854ff99-000000%40email.amazonses.com")

;;; funcs.el ends here
