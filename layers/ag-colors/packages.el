;; -*-lexical-binding: t -*-
;;; packages.el --- ag-colors layer packages

(defconst ag-colors-packages
  '(circadian
    base16-theme
    (ag-themes :location local)))

(defun ag-colors/init-base16-theme ()
  (use-package base16-theme))

(defun ag-colors/init-ag-themes ()
  (use-package ag-themes))

(defun ag-colors/init-circadian ()
  (use-package circadian
    :after ag-themes
    :config
    (setq
     ;; North of TX
     calendar-latitude 33.16
     calendar-longitude -96.93
     circadian-themes '((:sunrise . ag-themes-spacemacs-light)
                        (:sunset  . ag-themes-base16-ocean)))

    (setf dotspacemacs-themes (seq-map 'cdr circadian-themes))

    (run-at-time "3 sec" nil #'circadian-setup)))
