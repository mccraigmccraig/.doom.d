;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")
(setq user-full-name "mccraigmccraig of the clan mccraig"
      user-mail-address "mccraigmccraig@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 12)
      doom-variable-pitch-font (font-spec :family "Fira Code" :size 13))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; don't forget to load Doom modeline icon fonts with:
;; M-x nerd-icons-install-fonts

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'distinguished)
(setq doom-theme 'modus-vivendi)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; my customisations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; trivia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; black background plz - this makes all backgrounds black,
;; but obvs royally screws up light themes - i don't care
;; i just want max contrast dark themes
;; (custom-set-faces '(default ((t (:background "#000000")))))

;; never insert a tab character
(setq tab-always-indent t)

;; mark long lines everywhere
;; (setq whitespace-style
;;       '(face indentation tabs tab-mark trailing lines-tail))
(setq whitespace-line-column 80)

(use-package! whitespace
  :config
  (setq
   whitespace-style '(face indentation tabs tab-mark spaces space-mark trailing lines-tail newline newline-mark)
   whitespace-display-mappings '(
                                 ;; (space-mark   ?\     [?\u00B7]     [?.])
                                 ;; (space-mark   ?\xA0  [?\u00A4]     [?_])
                                 ;; (newline-mark ?\n    [?¬ ?\n])
                                 ;; (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])
                                 )))

(global-whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; smartparens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! smartparens

  (smartparens-global-strict-mode)

  ;; https://github.com/Fuco1/smartparens/blob/master/smartparens.el#L300
  (sp-use-smartparens-bindings)

  ;; undo the damage done by
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/config.el#L97
  ;; to double-quote autopairing - so we always get matching quotes
  (let ((unless-list '()))
    (sp-pair "\"" nil :unless unless-list))

  ;; undo the damage done by
  ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/config.el#L107
  ;; so we get matching parens when point is before a word again
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             :unless '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; vertico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! projectile
  :custom
  (projectile-enable-caching nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; vertico
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! vertico

  ;; default sort function
  (setq vertico-sort-function #'vertico-sort-history-alpha)

  ;; Enable vertico-multiform
  (vertico-multiform-mode)

  (setq vertico-multiform-categories
        '((file (vertico-sort-function . vertico-sort-alpha))))

  (setq vertico-multiform-commands
        '(;; present matching files in strict alpha order
          (projectile-find-file (vertico-sort-function . vertico-sort-alpha))

          ;; i would like to present project text-search results in strict alpha
          ;; order too, but even when the global default is #'vertico-sort-alpha
          ;; this doesn't work
          ;;
          ;; this doesn't work because consult-ripgrep
          ;; (which is used by these fns) does not send a "--sort path" arg to
          ;; rg (perhaps because it might be slower) - so path sorting is not
          ;; currently available to project-wide text-search
          (+default/search-project (vertico-sort-function . vertico-sort-alpha))
          (+vertico/project-search (vertico-sort-function . vertico-sort-alpha))
          (projectile-ripgrep (vertico-sort-function . vertico-sort-alpha)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display ace-window key in the modeline
;; using +light modeline now, so +modeline-ace-window below
;; is also required
(ace-window-display-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; give smartparens strict-mode a binding
(map!
 (:after smartparens
  :map smartparens-mode-map
  "C-M-z" #'smartparens-strict-mode))

;; give avy-jump a binding
(map!
 (:after avy
  :map doom-leader-map
  "j" #'avy-goto-char
  "C-j" #'avy-goto-char))

;; get rid of some annoying bindings
(map!
 ;; get rid of the annoying mouse-wheel text-scaling
 ;; which interacts badly with my screen-zoom
 "C-<wheel-down>" nil
 "C-<wheel-up>" nil
 "<pinch>" nil

 ;; get rid of C-z suspend-frame which i keep on hitting
 ;; when going for C-` +popup/toggle
 "C-z" nil)

(map!
 ;; don't show the messages buffer on minibuffer click
 (:map minibuffer-inactive-mode-map
       "<mouse-1>" #'ignore))

;; the default flycheck list stops the CIDER repl
;; buffer being a popup. consult-flycheck does not
;; do that
(map!
 (:after flycheck
  :map flycheck-command-map
  "l" #'consult-flycheck))

;; treesit

(use-package! treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)

         ;; More modes defined here...

         ("\\.ex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode)
         ("\\.heex\\'" . heex-ts-mode)
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")
               ;; 
               (elixir . ("https://github.com/elixir-lang/tree-sitter-elixir" "v0.3.4" "src"))
               (heex . ("https://github.com/phoenixframework/tree-sitter-heex" "v0.8.0" "src"))
               ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)
             ;; (elixir-mode . elixir-ts-mode)
             ))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

;; Do not forget to customize Combobulate to your liking:
;;
;;  M-x customize-group RET combobulate RET
;;
(use-package! combobulate
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook ((prog-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  :load-path ("/Users/mccraigmccraig/lib/combobulate"))


 ;;;; Code Completion
(use-package! corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

(use-package! flycheck
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(use-package! lsp-mode
  :diminish "LSP"
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           js-ts-mode) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter
  

  :init
  ;; need to wrap the language servers for this
  ;; (setq lsp-use-plists t)
  )

(after! lsp-mode
  ;; identify elixir files by mode
  (add-to-list 'lsp-language-id-configuration '(elixir-ts-mode . "elixir"))
  (add-to-list 'lsp-language-id-configuration '(heex-ts-mode . "elixir"))

  ;; elixir-mode doesn't seem to work very well on emacs30... but
  ;; running lexical in elixir-ts-mode seems fine
  (lsp-register-client (make-lsp-client
                        :new-connection (lsp-stdio-connection "/Users/mccraigmccraig/bin/lexical/_build/dev/package/lexical/bin/start_lexical.sh")
                        :activation-fn (lsp-activate-on "elixir")
                        :priority 1
                        ;; :major-modes '(elixir-ts-mode heex-ts-mode)
                        :server-id 'lexical-ls))

  ;; Add hooks to automatically start LSP for Elixir files
  (add-hook 'elixir-ts-mode-hook 'lsp)
  (add-hook 'heex-ts-mode-hook 'lsp))

(use-package! lsp-completion
  :no-require
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package! lsp-ui
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package! lsp-eslint
  :demand t
  :after lsp-mode)

;;; APHELEIA
;; auto-format different source code files extremely intelligently
;; https://github.com/radian-software/apheleia
(use-package! apheleia
  :diminish ""
  :defines
  apheleia-formatters
  apheleia-mode-alist
  :functions
  apheleia-global-mode
  :config
  (setf (alist-get 'prettier-json apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

;; aidermacs

(after! epa

  ;; gather the key password in the minibuffer
  (setq epg-pinentry-mode 'loopback)
  ;; this causes a "symbol's value as variable is void -----BEGIN" lint
  ;; error, but it's fine - because the file gets auto-decrypted
  (load-library "/Users/mccraigmccraig/.doom.d/secrets.el.gpg")

  (use-package! aidermacs
    :bind (("C-c a" . aidermacs-transient-menu))
    :config
                                        ; Set API_KEY in .bashrc, that will automatically picked up by aider or in elisp
    (setenv "ANTHROPIC_API_KEY" mccraigmccraig-anthropic-api-key)
                                        ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
    (setenv "OPENAI_API_KEY" mccraigmccraig-openai-api-key)
    :custom
                                        ; See the Configuration section below
    ;; (aidermacs-use-architect-mode t)
    (aidermacs-default-model "sonnet")))
