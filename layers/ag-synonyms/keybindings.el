(spacemacs/declare-prefix "xs" "synonyms")

(spacemacs/set-leader-keys
  "xss" 'synonyms-no-read
  "xsd" 'synonyms-definition-no-read
  "xsD" 'synonyms-definition
  "xst" 'ag/thesaurus-definition-no-read
  "xsT" 'ag/thesaurus-definition)

(evil-define-key 'visual synonyms-mode-map (kbd "R") 'ag/replace-word-other-window)
(evil-define-key 'normal synonyms-mode-map (kbd "R") 'ag/replace-word-other-window)
(evil-define-key 'normal synonyms-mode-map (kbd "q") 'quit-window)
