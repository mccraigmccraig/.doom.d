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
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

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

;; TODO don't work after upgrade
;; (def-modeline-var! +modeline-ace-window
;;   '(:eval (window-parameter (selected-window) 'ace-window-path)))

;; custom +light modeline (using +light option rather than full doom-modeline)
;; modified from
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/modeline/%2Blight.el#L525
;; to move flycheck left to prominence and add ace-window-path
;; (def-modeline! :main
;;   `(" "
;;     +modeline-ace-window
;;     " "
;;     (+modeline-checker ("" +modeline-checker " "))
;;     +modeline-matches
;;     +modeline-buffer-identification
;;     +modeline-position)
;;   `(""
;;     mode-line-misc-info
;;     +modeline-modes
;;     (vc-mode ("  "
;;               , ;; (all-the-icons-octicon "git-branch" :v-adjust 0.0)
;;               vc-mode " "))
;;     "  "
;;     +modeline-encoding))

;; (set-modeline! :main 'default)

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

(with-eval-after-load 'eglot
  (setf (alist-get '(elixir-mode elixir-ts-mode heex-ts-mode)
                   eglot-server-programs
                   nil nil #'equal)
        (if (and (fboundp 'w32-shell-dos-semantics)
                 (w32-shell-dos-semantics))
            '("language_server.bat")
          (eglot-alternatives
           '("/Users/mccraigmccraig/bin/lexical/_build/dev/package/lexical/bin/start_lexical.sh")))))

;; this doesn't set up the auto-mode-alist mappings for
;; elixir unfortunately - the elixir-mode stuff clashes
;; and overrides...
(use-package! elixir-ts-mode
  :mode (("\\.heex\\'" . heex-ts-mode)
         ("\\.ex\\'" . elixir-ts-mode))

  :config
  (add-hook! '(elixir-ts-mode-hook) #'lsp!))

;; aidermacs

(after! epa

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
