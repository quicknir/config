;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

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
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                     auto-completion-enable-help-tooltip 'manual)
     git
     (helm :variables
           helm-use-fuzzy nil)
     version-control
     ycmd
     syntax-checking
     ;; (with-eval-after-load 'yasnippet
     ;;   (setq yas-snippet-dirs (remq 'yas-installed-snippets-dir yas-snippet-dirs)))

     ;; Languages
     emacs-lisp
     cpp2
     (python :variables
             python-fill-column 99)
     html
     markdown
     csv
     ;; Only load/config json related packages
     (javascript :packages flycheck json-mode json-snatcher web-beautify)
     lua
     org
     yaml

     ;; Applications
     ranger
     treemacs
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'ansi-term
            shell-default-term-shell "zsh")

     ;; Only used for multi-line f and t
     (evil-snipe :variables
            evil-snipe-enable-alternate-f-and-t-behaviors t)

     ;; Customizations
     worse
     no-dots
     terminal
     mc-column
     pretty-git-modeline
     evil-mini
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(smartparens
                                    evil-lisp-state)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
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
   ;; (default 5)
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default nil)
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-light
                         spacemacs-dark)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Input Mono"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.6)
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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift nil
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
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
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy nil
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
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
   dotspacemacs-fullscreen-use-non-native nil
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
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
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
   dotspacemacs-folding-method 'origami
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
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
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (setq custom-file (concat (file-name-directory dotspacemacs-filepath) "customize.el"))
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Make C-/ expand yasnippet if available, else go into company
  ;; Must unbind undo tree first
  (with-eval-after-load 'undo-tree
      (define-key undo-tree-map (kbd "C-/") nil))
  (defun nir-yasnippet-expand-or-complete ()
    (interactive)
    (unless (call-interactively 'yas-expand) (call-interactively 'company-yasnippet)))
  ;; Must bind in global map, else undo tree stops loading
  (with-eval-after-load 'yasnippet
      (define-key global-map (kbd "C-/") 'nir-yasnippet-expand-or-complete))

  (defun nir-past-closers ()
    (interactive)
    (skip-syntax-forward ")"))

  ;; Some minor mode customizations
  (electric-pair-mode 1)
  (purpose-mode -1)
  (evil-escape-mode -1)
  (spaceline-toggle-buffer-encoding-abbrev-off)
  (ranger-override-dired-mode t)
  (setq helm-swoop-split-window-function 'spacemacs//display-helm-window)
  (setq persp-add-buffer-on-after-change-major-mode t)
  (setq evil-mc-mode-line-prefix "Ⓔ")
  (setq ycmd-mode-line-prefix "Ⓨ")
  (setq helm-always-two-windows nil)

  (setq helm-for-files-preferred-list '(worse-helm-source-projectile-files-list
                                        helm-source-buffers-list
                                        helm-source-recentf))

  (evil-define-key '(normal motion) global-map (kbd ";") 'helm-multi-files)

  ;; Treemacs
  (with-eval-after-load "treemacs"
    (treemacs-map-icons-with-auto-mode-alist
     '(".h")
     '((c-mode . treemacs-icon-c)
       (c++-mode . treemacs-icon-cpp))))

  ;; ycmd setup
  (set-variable 'ycmd-server-command '("python" "ycmd"))

  (add-hook 'python-mode-hook 'ycmd-mode)
  (setq company-ycmd-request-sync-timeout 1.0)
  (setq company-idle-delay 0.2)
  (global-set-key (kbd "<C-tab>") 'ycmd/manual-semantic-company-completer)

  ;; Rebind surround to S instead of s, so we can use s for avy
  (evil-define-key 'operator evil-surround-mode-map "S" 'evil-surround-edit)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)

  ;; avy setup
  (evil-define-key '(normal motion) global-map "s" 'avy-goto-char-timer)
  (evil-define-key '(visual operator) evil-surround-mode-map "s" 'avy-goto-char-timer)
  (setq avy-timeout-seconds 0.35)

  (evil-define-key '(normal motion visual operator) global-map (kbd "C-;") 'avy-goto-line)

  (setq avy-all-windows nil)

  ;; Setup ace-jump-helm-line, for quick selection inside helm
  (setq ace-jump-helm-line-default-action 'select)
  (setq ace-jump-helm-line-select-key ?e)
  (setq ace-jump-helm-line-move-only-key ?o)
  (setq ace-jump-helm-line-persistent-key ?p)

  (eval-after-load "helm"
    '(define-key helm-map (kbd "C-;") 'ace-jump-helm-line))

  ;; snipe; because I'm using alternate f/t behavior if I don't disable highlighting then
  ;; f<SPC> becomes very slow, for example
  ;; Note that in addition to multi-line, also allows repeated f/t/F/T presses to repeat
  ;; the seek. This effectively frees up ;, a home row key!
  (setq evil-snipe-enable-incremental-highlight nil)
  (setq evil-snipe-enable-highlight nil)
  ;; Disable all keybindings other than f/t
  (evil-snipe-mode -1)

  ;; Configuration for shell
  (evil-define-key 'insert term-mode-map (kbd "C-r") 'term-send-raw)
  (evil-define-key 'insert term-raw-map (kbd "C-r") 'term-send-raw)
  (evil-define-key 'insert term-mode-map (kbd "C-f") 'term-send-raw)
  (evil-define-key 'insert term-raw-map (kbd "C-f") 'term-send-raw)

  ;; Projectile
  (setq projectile-enable-caching t)

  ;; cc-mode
  (c-add-style "my-style"
               '("stroustrup"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 4)
                 (c-offsets-alist . (
                                     (innamespace . -)
                                     ))
                 ))

  (defun my-c++-mode-hook ()
    (c-set-style "my-style")
    )

  (add-hook 'c++-mode-hook 'my-c++-mode-hook)

  ;; evil mc
  (global-evil-mc-mode 1)
  ;; Fix for bug: https://github.com/gabesoft/evil-mc/issues/70
  ;; https://github.com/emacs-evil/evil/issues/864
  (add-hook 'evil-mc-after-cursors-deleted
            (lambda ()
              (setq evil-was-yanked-without-register t)))

  ;; This function can be used to make any helm command automatically follow
  (defun followize (command source)
    (lexical-let ((hc command)
                  (s source))
      (lambda ()
        (interactive)
        (let ((prev-follow-val (helm-attr 'follow s)))
          (helm-attrset 'follow 1 s)
          (call-interactively hc)
          (helm-attrset 'follow prev-follow-val s)))))

  ;; use helm-ag with follow in place of swoop
  (require 'helm-ag)

  (spacemacs/set-leader-keys "sb" (followize 'helm-do-ag-buffers helm-source-do-ag))
  (spacemacs/set-leader-keys "sB" (followize 'spacemacs/helm-buffers-do-ag-region-or-symbol helm-source-do-ag))
  (spacemacs/set-leader-keys "ss" (followize 'helm-do-ag-this-file helm-source-do-ag))
  (spacemacs/set-leader-keys "sS" (followize 'spacemacs/helm-file-do-ag-region-or-symbol helm-source-do-ag))

  ;; Follow in imenu
  (require 'helm-imenu)
  (setq helm-source-imenu
        (helm-make-source "Imenu" 'helm-imenu-source
          :fuzzy-match helm-imenu-fuzzy-match
          :follow 1))
  (setq helm-source-imenu-all
        (helm-make-source "Imenu in all buffers" 'helm-imenu-source
          :candidates 'helm-imenu-candidates-in-all-buffers
          :fuzzy-match helm-imenu-fuzzy-match
          :follow 1))

  ;; Window and buffer movement customizations, get this merged into spacemacs
  (spacemacs/set-leader-keys "wD" 'delete-other-windows)
  (spacemacs/set-leader-keys "bD" 'spacemacs/kill-other-buffers)

  ;; By default, d and x do the same thing in visual mode. Give x a more useful
  ;; purpose: delete to black hole register, so default register does not get
  ;; get overriden
  (evil-define-key  'visual global-map "x"
    (lambda () (interactive)
      (evil-use-register ?_)
      (call-interactively 'evil-delete-char) ))

  (defun helm-open-buffers-in-windows (buffers)
    (if (= (length buffers) 1) (set-window-buffer nil (car buffers))
      (let ((cur-win 1)
            (num-windows (length (window-list))))
        (cl-loop for buffer in buffers
                 do (when (<= cur-win num-windows)
                      (set-window-buffer (winum-get-window-by-number cur-win) buffer)
                      (setq cur-win (+ cur-win 1)))))))

  (require 'helm-projectile)
  (defun helm-find-files-windows (candidate)
    (let* ((files (helm-marked-candidates))
           (buffers (mapcar 'find-file-noselect files)))
      (helm-open-buffers-in-windows buffers)))

  ;; (helm-add-action-to-source "open in 1" 'helm-find-files-windows helm-source-projectile-files-list)
  ;; (helm-add-action-to-source "open in 1" 'helm-find-files-windows helm-source-find-files)
  (define-key helm-projectile-find-file-map (kbd "RET")
    (lambda ()(interactive)
      (helm-exit-and-execute-action 'helm-find-files-windows)))

  ;; Stotp treemacs from messing up numbering. Bind SPC caps-lock (esc)
  ;; to select treemacs
  (add-to-list 'winum-ignored-buffers " *Treemacs-Framebuffer-1*")
  (spacemacs/set-leader-keys "<escape>" 'treemacs-select-window)

  (setq evil-v$-gets-eol nil)

  (evil-define-motion evil-end-of-line (count)
    "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
    :type inclusive
    (move-end-of-line count)
    (when evil-track-eol
      (setq temporary-goal-column most-positive-fixnum
            this-command 'next-line))
    (unless (and (evil-visual-state-p) evil-v$-gets-eol)
      (evil-adjust-cursor)
      (when (eolp)
        ;; prevent "c$" and "d$" from deleting blank lines
        (setq evil-this-type 'exclusive))))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
