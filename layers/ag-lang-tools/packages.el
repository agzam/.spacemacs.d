(defconst ag-lang-tools-packages
  '((mw-thesaurus
     :location (recipe :fetcher github :repo "agzam/mw-thesaurus.el")
     ;; local
     )

    ;; sdcv-mode is for browsing Stardict format dictionaries in Emacs
    ;;
    ;; to get Webster’s Revised Unabridged Dictionary
    ;; 1) download it from https://s3.amazonaws.com/jsomers/dictionary.zip
    ;; 2) unzip it twice and put into ~/.stardict/dic
    ;; 3) Install sdcv, a command-line utility for accessing StarDict dictionaries
    ;;
    ;; you can find more dicts in stardict format here: http://download.huzheng.org/dict.org/
    ;; don't get the package from MELPA - it's been reported broken
    (sdcv-mode :location (recipe
                          :fetcher github
                          :repo "gucong/emacs-sdcv"))
    google-translate
    (keytar :location
     (recipe :fetcher github
             :repo "emacs-grammarly/keytar"))
    (lsp-grammarly
     :location
     (recipe :fetcher github :repo "emacs-grammarly/lsp-grammarly"))

    define-it))

(defun ag-lang-tools/init-keytar ()
  (use-package keytar
    :config
    (require 'keytar)))

(defun ag-lang-tools/init-lsp-grammarly ()
  (use-package lsp-grammarly
    :hook ((text-mode . lsp)
           (markdown-mode . lsp))
    :init
    (setq lsp-grammarly-auto-activate nil)
    :config
    (setq lsp-grammarly-domain "technical"
          lsp-grammarly-audience "expert")
    (spacemacs/set-leader-keys
      "xlg" #'lsp-grammarly-check-grammar))
  (with-eval-after-load 'lsp-grammarly
    (setq lsp-grammarly-active-modes (remove 'org-mode lsp-grammarly-active-modes))))

(defun ag-lang-tools/init-mw-thesaurus ()
  (use-package mw-thesaurus
    ;; :demand t
    :config
    (define-key mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
    (add-hook 'mw-thesaurus-mode-hook 'variable-pitch-mode)
    (spacemacs/set-leader-keys
      "xlm" #'mw-thesaurus-lookup-dwim)

    (add-to-list
     'display-buffer-alist
     `(,mw-thesaurus-buffer-name
       (display-buffer-reuse-window
        display-buffer-in-direction)
       (direction . right)
       (window . root)
       (window-width . 0.3)))))

(defun ag-lang-tools/init-sdcv-mode ()
  (use-package sdcv-mode
    :config
    (add-hook 'sdcv-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

    (defun sdcv-search-at-point ()
      (interactive)
      (sdcv-search (ag/region-or-word-at-point-str) nil nil t))

    (spacemacs/set-leader-keys "xll" #'sdcv-search-at-point)

    (evil-define-key 'normal sdcv-mode-map "q" #'sdcv-return-from-sdcv)
    (evil-define-key 'normal sdcv-mode-map "n" #'sdcv-next-entry)
    (evil-define-key 'normal sdcv-mode-map "p" #'sdcv-previous-entry)
    (evil-define-key 'normal sdcv-mode-map (kbd "RET") #'sdcv-search-at-point)
    (evil-define-key 'normal sdcv-mode-map "a" #'sdcv-search-at-point)

    (add-to-list
     'display-buffer-alist
     `(,sdcv-buffer-name
       (display-buffer-reuse-window
        display-buffer-in-direction)
       (direction . right)
       (window . root)
       (window-width . 0.25)))))

(defun ag-lang-tools/post-init-google-translate ()
  (setq google-translate-pop-up-buffer-set-focus t
        google-translate-default-source-language "ru"
        google-translate-default-target-language "en")

  ;; to use 'listen' feature of google-translate, on Mac:
  ;; brew install mplayer-osx-extended
  ;; ln -s '/Applications/MPlayer OSX Extended.app/Contents/Resources/Binaries/mpextended.mpBinaries/Contents/MacOS/mplayer' /usr/local/bin/mplayer
  (setf google-translate-listen-program
        (if (eq system-type 'darwin)
            "/usr/local/bin/mplayer"
          "/usr/bin/mplayer"))

  (setq google-translate-input-method-auto-toggling t
        google-translate-preferable-input-methods-alist
        '((nil . ("en"))
          (russian-computer . ("ru"))))

  ;; it doesn't pop to the buffer automatically
  (defun google-translate--find-buffer (x)
    (pop-to-buffer "*Google Translate*"))

  (advice-add 'google-translate-buffer-output-translation :after #'google-translate--find-buffer)

  (add-to-list
   'display-buffer-alist
   '("\\*Google Translate\\*"
     (display-buffer-reuse-window
      display-buffer-in-direction)
     (direction . right)
     (window . root)
     (window-width . 0.25))))

(defun ag-lang-tools/init-define-it ()
  (use-package define-it
    :config
    (setq
     define-it-show-google-translate nil
     define-it-show-header nil)

    (spacemacs/defer-until-after-user-config  ; otherwise, spacemacs-default layer would override the binding
     (lambda ()                               ; and set it to `duplicate-line-or-region', and it's pretty useles for me
       (spacemacs/set-leader-keys "xld" #'define-it-at-point)))

    ;; it doesn't pop to the buffer automatically, when definition is fetched
    (defun define-it--find-buffer (x)
      (let ((buf (format define-it--buffer-name-format define-it--current-word)))
        (pop-to-buffer buf)))

    (advice-add 'define-it--in-buffer :after #'define-it--find-buffer)
    (add-to-list
     'display-buffer-alist
     '("\\*define-it:"
       (display-buffer-reuse-window
        display-buffer-in-direction)
       (direction . right)
       (window . root)
       (window-width . 0.25)))))

(with-eval-after-load 'ispell
  (setq flyspell-issue-message-flag nil)    ; printing a message for every word has a negative performance impact
  (setq ispell-program-name "aspell")
  ;; aspell suggestion mode
  (add-to-list 'ispell-extra-args "--sug-mode=bad-spellers")

  ;; Change dictionary with the input-method
  (defun change-dict-after-toggle-input (_ _)
    (ispell-change-dictionary
     (if (string= current-input-method "russian-computer")
         "ru"
       nil)))

  (advice-add 'toggle-input-method :after 'change-dict-after-toggle-input))
