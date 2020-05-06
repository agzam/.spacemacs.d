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
    (emacs-grammarly :location (recipe
                                :fetcher github
                                :repo "mmagnus/emacs-grammarly"))))

(defun ag-lang-tools/init-mw-thesaurus ()
  (use-package mw-thesaurus
    :demand t
    :config
    (define-key mw-thesaurus-mode-map [remap evil-record-macro] #'mw-thesaurus--quit)
    (add-hook 'mw-thesaurus-mode-hook 'variable-pitch-mode)
    (spacemacs/set-leader-keys
      "xlm" #'mw-thesaurus-lookup-at-point
      "xAg" #'add-global-abbrev
      "xAl" #'add-mode-abbrev)))

(defun ag-lang-tools/init-sdcv-mode ()
  (use-package sdcv-mode
    :demand t
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

(defun ag-lang-tools/init-emacs-grammarly ()
  (use-package emacs-grammarly
    :config
    (spacemacs/set-leader-keys
      "xlg" #'grammarly-save-region-and-run)))

(with-eval-after-load 'ispell
  (setq ispell-program-name "aspell")
  ;; aspell suggestion mode - ultra-fast
  (add-to-list 'ispell-extra-args "--sug-mode=ultra"))

(with-eval-after-load 'google-translate
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
