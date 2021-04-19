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
     :location (recipe :fetcher github :repo "emacs-grammarly/lsp-grammarly"))))

(defun ag-lang-tools/init-keytar ()
  (use-package lsp-grammarly
    :hook (text-mode . (lambda ()
                         (require 'lsp-grammarly)
                         (lsp)))
    :config
    (setq lsp-grammarly-domain "technical"
          lsp-grammarly-audience "expert")))

(defun ag-lang-tools/init-lsp-grammarly ()
  (use-package keytar
    :config
    (require 'keytar)))

(defun ag-lang-tools/init-mw-thesaurus ()
  (use-package mw-thesaurus
    ;; :demand t
    :config
    (define-key mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
    (add-hook 'mw-thesaurus-mode-hook 'variable-pitch-mode)
    (spacemacs/set-leader-keys
      "xlm" #'mw-thesaurus-lookup-dwim
      ;; "xAg" #'add-global-abbrev
      ;; "xAl" #'add-mode-abbrev
      )))

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
    (evil-define-key 'normal sdcv-mode-map "a" #'sdcv-search-at-point)))

(defun ag-lang-tools/post-init-google-translate ()
  (setq google-translate-pop-up-buffer-set-focus t
        google-translate-default-source-language "ru"
        google-translate-default-target-language "en")

  ;; to listen. on Mac:
  ;; brew cask install mplayer-osx-extended
  ;; ln -s '/Applications/MPlayer OSX Extended.app/Contents/Resources/Binaries/mpextended.mpBinaries/Contents/MacOS' /usr/local/bin/mplayer
  (setf google-translate-listen-program
        (if (eq system-type 'darwin)
            "/usr/local/bin/mplayer"
          "/usr/bin/mplayer"))

  (setq google-translate-input-method-auto-toggling t
        google-translate-preferable-input-methods-alist
        '((nil . ("en"))
          (russian-computer . ("ru")))))

(with-eval-after-load 'ispell
  (setq flyspell-issue-message-flag nil) ; printing a message for every word has a negative performance impact
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
