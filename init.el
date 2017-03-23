;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/") ;
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '((org :variables
          org-enable-reveal-js-support t
          org-enable-bootstrap-support t
          org-enable-github-support t
          :packages (not org-present))
     ;; (spacemacs-ui-visual :packages (not fancy-battery))
     ;; ---- Languages -----
     csv
     yaml
     emacs-lisp
     javascript
     (html :packages (not pug-mode slim-mode))
     (markdown :variables markdown-live-preview-engine 'vmd)
     (shell-scripts :packages (not fish-mode))
     clojure
     lua
     ;; python
     ;; react
     ;; (ruby :variables ruby-enable-enh-ruby-mode t
     ;;                  ruby-version-manager 'rvm)
     haskell

     ;; --- Editor  ----
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)

     syntax-checking
     spell-checking

     ;; ---- Tools ----
     helm
     (shell :variables
            shell-enable-smart-eshell t
            comint-scroll-show-maximum-output nil
            comint-move-point-for-output nil)

     fasd
     (ibuffer :variables ibuffer-old-time 8)
     osx
     ;; (semantic :disabled-for '(emacs-lisp org))
     restclient
     emoji
     ;; ranger
     search-engine
     imenu-list
     docker

     ;; ---- Version control ----
     git
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)

     (github :packages (not magit-gh-pulls)
             :variables
             ;; view your Gist using `browse-url` after it is created 
             gist-view-gist t)

     ;; --- My own layers ----
     ag-dired
     ag-general
     ag-dash
     ag-synonyms
     ag-org
     ag-clojure
     ag-cal
     ;; ag-jira
     ;; ag-4clojure
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(dired+
                                      flycheck-package
                                      helm-flycheck
                                      tern-auto-complete
                                      string-inflection
                                      writeroom-mode
                                      (base16-ocean-dark :location local))
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 10
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((warnings . 5) (recents . 5) (projects . 5) (todos . 10) (bookmarks . 5) (agenda . 5))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive nil
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(base16-ocean-dark zenburn sanityinc-tomorrow-eighties obsidian solarized-light)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 0.5)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t
   ;; If non-nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header t
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 80
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title nil
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide nil
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil))

(defun dotspacemacs/user-init ()
  (add-to-list 'custom-theme-load-path "~/.spacemacs.d/themes")

  (setq-default
   menu-bar-mode t
   exec-path-from-shell-check-startup-files nil
   ;; Editor
   line-spacing 6
   left-fringe-width 5
   right-fringe-width 0
   evil-escape-key-sequence "jk"
   evil-escape-delay 0.1
   fill-column 120
   frame-background-mode 'dark

   ;; Shell
   ;; system-uses-terminfo nil
   ;; shell-default-shell 'shell
   eyebrowse-keymap-prefix (kbd "C-x C-w")
   evil-search-module 'isearch
   )

  (setq
   ns-auto-hide-menu-bar nil
   package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("marmalade" . "https://marmalade-repo.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))

   dotspacemacs-default-layout-name "•"

   ;; better looking font in books
   doc-view-resolution 144))

(defun dotspacemacs/user-config ()
  "Configuration function for user code. This function is called at the very end of Spacemacs initialization after layers configuration. You are free to put any user code."
  (setq
   ;;;; Editor
   powerline-default-separator nil
   powerline-center-theme t
   avy-timeout-seconds 0.4
   aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)                       ;;;; ace-windows instead of characters shows number
   linum-format "%3d\u2502"                                    ;;;; nicer line-numbers
   frame-title-format "%f"                                     ;;;; full filepath in the title
   spacemacs-show-trailing-whitespace nil
   tab-width 4
   mouse-wheel-scroll-amount '(0.02)
   mouse-wheel-progressive-speed nil
   scroll-margin 2
   smooth-scroll-margin 2
   default-input-method 'russian-computer
   google-translate-default-source-language "ru"
   google-translate-default-target-language "en"

   ;;;; Helm
   helm-echo-input-in-header-line nil
   helm-ff--deleting-char-backward t
   helm-follow-mode-persistent t
   ;; helm-buffer-details-flag nil        ;; Always show details in buffer list when non--nil.

   ;;;; Misc
   vc-follow-symlinks t
   diff-hl-side 'left
   use-dialog-box nil
   apropos-sort-by-scores t
   sass-indent-offset 2

   ;; don't quit on esc
   evil-escape-excluded-major-modes '(magit-status-mode
                                      magit-diff-mode
                                      magit-refs-mode
                                      help-mode paradox-menu-mode) 
   ranger-override-dired nil
   delete-by-moving-to-trash nil
   magit-show-refs-arguments '("--sort=-committerdate")
   magit-delete-by-moving-to-trash nil)

   ;; (pupo-mode -1)
   ;; (purpose-mode -1)
   (add-hook 'prog-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
   (spacemacs/toggle-mode-line-version-control-off)
   (spacemacs/toggle-mode-line-minor-modes-off)

  ;; ---------------
  ;; Shell, Term
  ;; ---------------
  (defun ag/shell-hook ()
    (setq-local global-hl-line-mode nil)
    (setq-local scroll-margin 0)
    (text-scale-decrease 0.1))

  (add-hook 'eshell-mode-hook #'ag/shell-hook)
  (add-hook 'term-mode-hook #'ag/shell-hook)
  (add-hook 'shell-mode-hook #'ag/shell-hook)

  (add-to-list 'sp-sexp-suffix '(js2-mode regex ""))

  ;; ---------------
  ;; Haskell
  ;; ---------------
  (add-to-list 'exec-path "~/Library/haskell/bin/")

  ;; ---------------
  ;; Text
  ;; ---------------
  (add-hook 'markdown-mode-hook #'flyspell-mode)

  ;; ---------------
  ;; css-mode sass-mode
  ;; ---------------
  (add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
  (defun ag/sass-mode-hook ()
    (auto-complete-mode 1)
    (rainbow-mode 1))

  (add-hook 'sass-mode-hook #'ag/sass-mode-hook)
  (with-eval-after-load 'sass (rvm-use-default))

  ;; auto-completion
  (with-eval-after-load 'auto-complete (add-to-list 'ac-dictionary-directories "~/.spacemacs.d/ac-dict"))

  ;; ------------
  ;; Javascript mode
  ;; ------------
  (setq js2-include-node-externs t
        js2-include-browser-externs t
        js2-include-global-externs t
        js2-global-externs (list "$" "window" "angular" "Rx" "_" "moment")
        coffee-tab-width 4
        ;; js-indent-level 4
        ;; js2-basic-offset 4
        )
  (add-hook 'js2-mode-hook #'flyspell-prog-mode)

  ;; Flycheck JSCS
  (flycheck-def-config-file-var flycheck-jscs
      javascript-jscs ".jscs.json" :safe #'stringp)
  (flycheck-define-checker javascript-jscs
    "A JavaScript code style checker. See URL `https://github.com/mdevils/node-jscs'."
    :command ("jscs" "--reporter"
              "checkstyle"
              (config-file "--config" flycheck-jscs)
              source):error-parser
              flycheck-parse-checkstyle
              :modes (js-mode js2-mode js3-mode):next-checkers
              (javascript-jshint))
  (defun jscs-enable ()
    (interactive)
    (add-to-list 'flycheck-checkers 'javascript-jscs))
  (defun jscs-disable ()
    (interactive)
    (setq flycheck-checkers (remove 'javascript-jscs flycheck-checkers)))
  (setq js2-strict-inconsistent-return-warning nil)

  ;; Flycheck coffeelint
  (setq flycheck-coffee-coffeelint-executable
        "~/.nvm/versions/node/v5.6.0/bin/node/bin/coffeelint")
  (setq flycheck-coffeelintrc "~/.coffeelintrc")
  (add-hook 'coffee-mode-hook #'flyspell-prog-mode)

  ;; (setenv "PATH" (concat (getenv "PATH") ":~/.nvm/versions/node/v5.6.0/bin/node"))
  ;; (setq exec-path (append exec-path '("~/.nvm/versions/node/v5.6.0/bin/node/bin")))
  ;; get rid of stupid tern-port files
  (setq tern-command '("tern" "--no-port-file"))

  ;; ---------
  ;; Magit
  ;; ---------
  ;; don't exit magit on escape-sequence and don't bury its buffer on Esc
  (with-eval-after-load 'evil-magit
    (evil-magit-define-key 'normal 'magit-mode-map "<escape>" nil))

  ;; --------
  ;; dired / ranger
  ;; -----
  (setq dired-omit-files "^\\.?#\\|^\\.DS_Store$")
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  ;; (setq-default split-height-threshold 150 ;;http://stackoverflow.com/questions/23207958/how-to-prevent-emacs-dired-from-splitting-frame-into-more-than-two-windows
  ;;               split-width-threshold 200)

  ;; ----------
  ;; Helm
  ;; ----------
  (with-eval-after-load 'helm-ag (setq helm-ag-use-agignore t))

  ;; -----------
  ;; Perspectives
  ;; ----------
  (setq-default perspective-enable-persp-projectile t
                persp-auto-save-opt 0)

  ;; -----------
  ;; Yasnippet
  ;; ----------
  (with-eval-after-load 'yasnippet (add-to-list 'yas-snippet-dirs "~/.spacemacs.d/snippets"))

  ;; -----------
  ;; Backups
  ;; ----------
  (setq version-control t
        backup-by-copying t
        kept-new-versions 64
        kept-old-versions 0
        delete-old-versions nil)
  (setq backup-directory-alist '(("." . ".bak"))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

