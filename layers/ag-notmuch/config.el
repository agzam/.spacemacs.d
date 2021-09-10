;;; config.el --- ag-notmuch layer configuration vars
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

(defvar notmuch-message-deleted-tags '("+deleted" "-inbox" "-unread")
  "Tags applied when deleting a message.")

(defvar notmuch-spacemacs-layout-name "@Notmuch"
  "Name used in the setup for `spacemacs-layouts' micro-state")

(defvar notmuch-spacemacs-layout-binding "n"
  "Binding used in the setup for `spacemacs-layouts' micro-state")

(defvar notmuch-modes
  '(notmuch-hello-mode
    notmuch-message-mode
    notmuch-search-mode
    notmuch-show-mode
    notmuch-tree-mode)
  "Modes that are associated with notmuch buffers.")

;;; config.el ends here
