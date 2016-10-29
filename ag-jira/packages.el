(setq ag-jira-packages
    '(org-jira))

(setq ag-jira-excluded-packages '())

(defun ag-jira/init-org-jira ()
    (use-package org-jira
      :defer t

      :init
      (setq jiralib-url "http://svrqc.mobitor.com:8080")
      ))

