(setq ag-jira-packages
    '(org-jira))

(defun ag-jira/init-org-jira ()
    (use-package org-jira
      :defer t
      :init
      (setq jiralib-url "")))

