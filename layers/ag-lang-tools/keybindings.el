(spacemacs/declare-prefix "xs" "synonyms")

(spacemacs/set-leader-keys
  "xlm" #'mw-thesaurus/lookup-at-point)

(defun sdcv-search-at-point ()
  (interactive)
  (sdcv-search (ag/region-or-word-at-point-str) nil nil t))

(spacemacs/set-leader-keys "xll" #'sdcv-search-at-point)

(evil-define-key 'normal sdcv-mode-map "q" #'sdcv-return-from-sdcv)
(evil-define-key 'normal sdcv-mode-map "n" #'sdcv-next-entry)
(evil-define-key 'normal sdcv-mode-map "p" #'sdcv-previous-entry)
(evil-define-key 'normal sdcv-mode-map (kbd "RET") #'sdcv-search-at-point)
(evil-define-key 'normal sdcv-mode-map "a" #'sdcv-search-at-point)
