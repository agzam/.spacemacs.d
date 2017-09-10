(defconst mw-thesaurus-packages
  '((mw-thesaurus :location (recipe
                             :fetcher github
                             :repo "agzam/mw-thesaurus.el"))))

(defun mw-thesaurus/init-mw-thesaurus ()
  (use-package mw-thesaurus
    :ensure t))
