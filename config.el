;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "mccraigmccraig of the clan mccraig"
      user-mail-address "mccraigmccraig@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-acario-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
(setq whitespace-style
      '(face indentation tabs tab-mark trailing lines-tail))
(setq whitespace-line-column 80)
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

;; the doom default is `relative-from-project, but that
;; often causes stuff on the RH end of the modeline to get knocked
;; off when showing 3 windows side-by-side,
;; so this is more compact
;; https://github.com/seagle0128/doom-modeline
;;
;;(setq doom-modeline-buffer-file-name-style 'truncate-with-project)
;;(setq doom-modeline-buffer-file-name-style 'auto)
;;(setq doom-modeline-buffer-file-name-style 'relative-to-project)

;; display ace-window key in the modeline
;; using +light modeline now, so +modeline-ace-window below
;; is also required
(ace-window-display-mode)

(def-modeline-var! +modeline-ace-window
  '(:eval (window-parameter (selected-window) 'ace-window-path)))

;; custom +light modeline (using +light option rather than full doom-modeline)
;; modified from
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/modeline/%2Blight.el#L525
;; to move flycheck left to prominence and add ace-window-path
(def-modeline! :main
  `(" "
    +modeline-ace-window
    " "
    (+modeline-checker ("" +modeline-checker " "))
    +modeline-matches
    +modeline-buffer-identification
    +modeline-position)
  `(""
    mode-line-misc-info
    +modeline-modes
    (vc-mode ("  "
              , ;; (all-the-icons-octicon "git-branch" :v-adjust 0.0)
              vc-mode " "))
    "  "
    +modeline-encoding))

(set-modeline! :main 'default)

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

;; doom has its own auto-revert stuff, so
;; this shouldn't be necessary... but i observed some
;; files not being auto-reverted when i thought they should be
;; ... update... this didn't achieve anything anyway
;; (setq auto-revert-use-notify t)
;; (global-auto-revert-mode t)


;; this doesn't set up the auto-mode-alist mappings for
;; elixir unfortunately - the elixir-mode stuff clashes
;; and overrides...
(use-package! elixir-ts-mode
  :mode (("\\.heex\\'" . heex-ts-mode)
         ("\\.ex\\'" . elixir-ts-mode))

  :config
  (add-hook! '(elixir-ts-mode-hook) #'lsp!))

;; the doom elixir bindigs always require elixir-mode atm
(after! elixir-mode

  (setq lsp-elixir-ls-server-dir "~/bin/elixir-ls")
  (setq lsp-elixir-local-server-command "~/bin/elixir-ls/language_server.sh")

  ;; this doesn't work
  ;; (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-ts-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.heex\\'" . elixir-ts-mode))

  ;; and this gets the right mode, but stops the LSP working 🤔
  ;; (add-to-list 'major-mode-remap-alist

  ;;              '(elixir-mode . elixir-ts-mode))

  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; combobulate / tree-sitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; install a couple of tree-sitter langs
(after! treesit
  (add-to-list 'treesit-language-source-alist '(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

  (add-to-list 'treesit-language-source-alist '(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))


  (add-to-list 'treesit-language-source-alist '(elixir . ("https://github.com/elixir-lang/tree-sitter-elixir" "main" "src")))
  (add-to-list 'treesit-language-source-alist '(heex . ("https://github.com/phoenixframework/tree-sitter-heex" "main" "src")))
  )

(use-package! typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'lsp!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; combobulate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; should have another go at getting this running now that
;; treesit + lsp is working for typescript

;; `M-x combobulate' (default: `C-c o o') to start using Combobulate
;; (use-package treesit
;;   :preface
;;   (defun mp-setup-install-grammars ()
;;     "Install Tree-sitter grammars if they are absent."
;;     (interactive)
;;     (dolist (grammar
;;              '((css "https://github.com/tree-sitter/tree-sitter-css")
;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
;;                (python "https://github.com/tree-sitter/tree-sitter-python")
;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
;;                (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       ;; Only install `grammar' if we don't already have it
;;       ;; installed. However, if you want to *update* a grammar then
;;       ;; this obviously prevents that from happening.
;;       (unless (treesit-language-available-p (car grammar))
;;         (treesit-install-language-grammar (car grammar)))))

;;   ;; Optional, but recommended. Tree-sitter enabled major modes are
;;   ;; distinct from their ordinary counterparts.
;;   ;;
;;   ;; You can remap major modes with `major-mode-remap-alist'. Note
;;   ;; that this does *not* extend to hooks! Make sure you migrate them
;;   ;; also
;;   (dolist (mapping '((python-mode . python-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (typescript-mode . tsx-ts-mode)
;;                      (js-mode . js-ts-mode)
;;                      (css-mode . css-ts-mode)
;;                      (yaml-mode . yaml-ts-mode)))
;;     (add-to-list 'major-mode-remap-alist mapping))

;;   :config
;;   (mp-setup-install-grammars))

;; Do not forget to customize Combobulate to your liking:
;;
;;  M-x customize-group RET combobulate RET
;;
;; (use-package combobulate
;;   :after (treesit)
;;   :preface
;;   ;; You can customize Combobulate's key prefix here.
;;   ;; Note that you may have to restart Emacs for this to take effect!
;;   (setq combobulate-key-prefix "C-c o")

;;   ;; Optional, but recommended.
;;   ;;
;;   ;; You can manually enable Combobulate with `M-x
;;   ;; combobulate-mode'.
;;   :hook ((python-ts-mode . combobulate-mode)
;;          (js-ts-mode . combobulate-mode)
;;          (css-ts-mode . combobulate-mode)
;;          (yaml-ts-mode . combobulate-mode)
;;          (typescript-ts-mode . combobulate-mode)
;;          (tsx-ts-mode . combobulate-mode))
;;   ;; Amend this to the directory where you keep Combobulate's source
;;   ;; code.
;;   :load-path ("/Users/mccraig/.emacs.d/.local/combobulate"))
