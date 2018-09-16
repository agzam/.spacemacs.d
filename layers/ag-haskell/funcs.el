;;; funcs.el --- ag-haskell layer functions file.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/agzam/dot-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun window-list-sorted ()
  "Return window list sorted 'as-is'.

Problem of `window-list' and `window-list-1' that they return windows sorted in relation to current window.
This function ignores current window"
  (window-list nil nil (frame-first-window)))

;;; funcs.el ends here
