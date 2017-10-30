(global-set-key (kbd "C-x C-p") 'org-pomodoro)
(global-set-key (kbd "C-x p") 'org-pomodoro)

(evil-define-key 'insert org-mode-map (kbd "M-RET") 'ag/org-meta-return)
(evil-define-key 'insert org-mode-map (kbd "RET") 'org-return-indent)
(evil-define-key 'insert org-mode-map (kbd "<S-return>") 'org-return)

(evil-define-key 'normal org-mode-map "H" 'org-shiftleft)
(evil-define-key 'normal org-mode-map "L" 'org-shiftright)
(evil-define-key 'normal org-mode-map "J" 'org-shiftdown)
(evil-define-key 'normal org-mode-map "K" 'org-shiftup)
