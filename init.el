;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.
;; (setq-default quelpa-build-tar-executable "/usr/local/bin/gtar")
;; (setq package-check-signature nil)

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
   dotspacemacs-enable-lazy-installation nil

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/") ;
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   `(
     ;; ---- Languages -----
     csv yaml emacs-lisp lua javascript ag-haskell sql rust
     (html :packages (not pug-mode slim-mode))
     (markdown
      :packages (not mmm-mode)
      :variables markdown-live-preview-engine 'vmd)
     (shell-scripts :packages (not fish-mode))
     ;; --- Editor  ----
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     ;; ---- Tools ----
     ,(when (eq system-type 'darwin) 'osx)
     ,(when (eq system-type 'gnu/linux) 'common-lisp)
     multiple-cursors
     docker emoji fasd imenu-list restclient search-engine
     (ivy :variables
          ivy-fixed-height-minibuffer t
          ivy-enable-advanced-buffer-information t
          ivy-initial-inputs-alist nil
          ivy-re-builders-alist '((counsel-projectile-find-file . ivy--regex-fuzzy)
                                  (counsel-projectile-find-dir . ivy--regex-fuzzy)
                                  (ivy-switch-buffer . ivy--regex-fuzzy)
                                  (cider-repl-handle-shortcut . ivy--regex-fuzzy)
                                  (helpful-symbol . ivy--regex-fuzzy)
                                  (t . ivy--regex-plus))
          ivy-height 20
          ivy-read-action-function 'ivy-hydra-read-action ; until https://github.com/abo-abo/swiper/issues/2469 fixed
          )
     ;; (dash :variables
     ;;       helm-dash-docset-path
     ;;       (cond ((eq system-type 'darwin) "~/Library/Application\ Support/Dash/DocSets")))
     (shell :variables
            shell-default-shell 'shell
            shell-default-full-span nil)
     ;; --- My own layers ----
     (ag-clojure :variables
                 clojure-enable-clj-refactor t
                 clojure-enable-sayid t
                 clojure-enable-linters '(clj-kondo))
     ag-colors
     ag-dired
     ag-general
     (ag-lang-tools
      :variables spell-checking-enable-by-default nil)
     (ag-org
      :variables org-enable-github-support t)
     ag-mail
     ag-version-control
     ag-web
     (lsp
      :variables
      lsp-ui-sideline-enable nil
      lsp-ui-doc-enable nil
      lsp-ui-doc-position 'at-point
      lsp-enable-completion-at-point nil))

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(copy-as-format
                                      ivy-rich
                                      quelpa-use-package
                                      dired-narrow
                                      ivy-posframe)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(window-purpose)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default spacemacs-27.1.pdmp)
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes (let ((hr (nth 2 (decode-time))))
                         (if (or (< hr 8) (< 20 hr))         ; between 9PM and 8AM
                             '(base16-ocean spacemacs-light) ; load dark theme first
                           '(spacemacs-light base16-ocean)))

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom :separator nil :separator-scale 1)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("JetBrains Mono"
                               :size 15
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Home"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout t

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names t

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

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state t

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

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
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title nil

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide nil

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%t           ― · ―            %b"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (setq spacemacs-env-vars-file "~/.emacs.d/.spacemacs.env")
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

  (setq-default
   ;; menu-bar-mode t
   exec-path-from-shell-check-startup-files nil
   ;; Editor
   line-spacing 8
   left-fringe-width 6
   right-fringe-width 0
   evil-escape-key-sequence "kj"
   evil-esc-delay 0.3
   frame-background-mode 'dark
   eyebrowse-keymap-prefix (kbd "C-x C-x"))

  (setq
   evil-want-fine-undo nil
   ns-use-srgb-colorspace t
   evil-want-C-u-scroll nil
   ns-auto-hide-menu-bar nil
   exec-path-from-shell-variables '("PATH" "MANPATH" "NVM_DIR" "NODE_PATH" "HOMEBREW_GITHUB_API_TOKEN" "DEV")
   custom-file "~/.spacemacs.d/custom.el")

  (with-eval-after-load 'auto-highlight-symbol
   (spacemacs/toggle-automatic-symbol-highlight-on))

  (load custom-file)

  (setq doom-modeline-height 1
        doom-modeline-major-mode-icon t
        doom-modeline-icon (display-graphic-p)
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-major-mode-color-icon nil
        doom-modeline-mu4e t
        doom-modeline-buffer-encoding nil
        ))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (setq
   ;;;; Editor
   frame-resize-pixelwise t ; make sure `(maximize-frame)` leaves no borders
   powerline-default-separator nil
   powerline-center-theme nil
   avy-timeout-seconds 0.4
   winum-scope 'frame-local
   aw-keys '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9) ; ace-windows instead of characters shows number
   linum-format "%3d\u2502" ; nicer line-numbers
   fill-column 170
   tab-width 4
   mouse-wheel-scroll-amount '(0.02)
   mouse-wheel-progressive-speed nil
   scroll-margin 2
   smooth-scroll-margin 2
   default-input-method 'russian-computer
   scroll-margin 0
   abbrev-file-name "~/.spacemacs.d/abbrev_defs"
   save-abbrevs t ; save abbrevs upon exiting Emacs
   uniquify-buffer-name-style 'forward
   company-idle-delay 0.1
   company-show-numbers t
   writeroom-width 130
   sentence-end-double-space t

   ;;;; Helm
   helm-echo-input-in-header-line nil
   helm-ff--deleting-char-backward t
   helm-follow-mode-persistent nil
   helm-buffer-details-flag t ; Always show details in buffer list when non--nil.
   apropos-sort-by-scores t

   ;;;; Misc
   eldoc-echo-area-use-multiline-p 'always
   eldoc-idle-delay 0.25
   use-dialog-box nil
   dumb-jump-force-searcher 'rg ; https://github.com/jacktasia/dumb-jump#emacs-options
   ;; don't quit on esc or jk
   evil-escape-excluded-major-modes '(compilation-mode
                                      help-mode
                                      magit-diff-mode magit-log-mode magit-process-mode magit-refs-mode magit-revision-mode magit-stash-mode magit-status-mode mu4e-headers-mode
                                      mu4e-main-mode mu4e-view-mode
                                      org-agenda-mode
                                      paradox-menu-mode
                                      appropos-mode
                                      image-mode)
   Man-notify-method 'pushy
   ranger-override-dired nil
   delete-by-moving-to-trash nil)
  (global-hl-line-mode 0)
   ;;;; end setq

  (with-eval-after-load 'helm-ag (setq helm-ag-use-agignore t))

  (with-eval-after-load 'spaceline-segments
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (spaceline-toggle-purpose-off))

  (with-eval-after-load 'spaceline-config
    (spacemacs/toggle-mode-line-org-clock-on)
    (spacemacs/toggle-mode-line-version-control-off)
    (spacemacs/toggle-mode-line-minor-modes-off)
    (spacemacs/toggle-mode-line-responsive-off)

    (spaceline-define-segment buffer-id
      (if (buffer-file-name)
          (abbreviate-file-name (buffer-file-name))
        (powerline-buffer-id))))

  (add-hook 'prog-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
  ;; (add-hook 'abbrev-mode-hook #'read-abbrev-file)
  ;; (remove-hook 'evil-insert-state-exit-hook 'expand-abbrev)

  (with-eval-after-load 'auto-complete (add-to-list 'ac-dictionary-directories "~/.spacemacs.d/ac-dict"))

  ;; ---------------
  ;; Haskell
  ;; ---------------
  (add-to-list 'exec-path "~/Library/haskell/bin/")

  ;; ---------------
  ;; Text
  ;; ---------------
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook 'spacemacs/toggle-visual-line-navigation-on)

  ;; -----------
  ;; Perspectives
  ;; ----------
  (setq-default perspective-enable-persp-projectile t
                persp-auto-save-opt 0
                persp-switch-wrap nil
                persp-kill-foreign-buffer-behaviour 'kill)

  ;; -----------
  ;; Yasnippet
  ;; ----------
  (with-eval-after-load 'yasnippet (add-to-list 'yas-snippet-dirs "~/.spacemacs.d/snippets"))

  (remove-hook 'diff-mode-hook 'whitespace-mode)

  ;; -----------
  ;; Backups
  ;; ----------
  (setq version-control t
        backup-by-copying t
        kept-new-versions 64
        kept-old-versions 0
        delete-old-versions nil)
  (setq backup-directory-alist '(("." . ".bak")))
  (savehist-mode -1)

  ;; (magit-wip-after-save-mode 1)

  (with-eval-after-load 'helm
    ;; experimenting with helm in a separate frame
    ;; (setq helm-display-function 'helm-display-buffer-in-own-frame
    ;;       helm-display-buffer-reuse-frame t
    ;;       helm-use-undecorated-frame-option nil
    ;;       helm-display-buffer-width 150
    ;;       helm-display-buffer-height 20)
    ;; (setq helm-display-function 'helm-default-display-buffer)
    (setq which-key-sort-order 'which-key-prefix-then-key-order))

  (spacemacs|define-custom-layout "@dotfile"
    :binding "d"
    :body (find-file "~/dotfile.org/dotfile.org"))

  (run-at-time "2 sec" nil #'ag/adjust-themes)

  ;; TODO: remove when they fix https://github.com/emacsorphanage/helm-swoop/issues/77
  (with-eval-after-load 'helm-swoop
    (define-key helm-swoop-edit-map (kbd "C-c C-c") 'helm-swoop--edit-complete)
    (define-key helm-swoop-edit-map (kbd "C-c C-k") 'helm-swoop--edit-cancel))


  ;; make ivy stuff a bit readable in the minibuffer
  (defun minibuffer-line-spacing ()
    (setq-local line-spacing 6))
  (add-hook 'minibuffer-setup-hook #'minibuffer-line-spacing)

  ;; (if (< (nth 2 (decode-time)) 21)                             ; after 9PM
  ;;     (spacemacs/load-theme (nth 1 dotspacemacs-themes) nil t) ; load dark theme
  ;;   (spacemacs/load-theme (nth 0 dotspacemacs-themes) nil t))
  )
