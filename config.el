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

;;;; my customisations

;; black background plz - this makes all backgrounds black,
;; but obvs royally screws up light themes - i don't care
;; i just want max contrast dark themes
(custom-set-faces '(default ((t (:background "#000000")))))

;; never insert a tab character
(setq tab-always-indent t)

;; display ace-window key in the modeline
(ace-window-display-mode)

;; mark long lines everywhere
(setq whitespace-style
      '(face indentation tabs tab-mark trailing lines-tail lines))
(setq whitespace-line-column 80)
(global-whitespace-mode)

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

(map!
 (:after smartparens
  :map smartparens-mode-map
  "C-M-z" #'smartparens-strict-mode))

(map!
 (:after avy
  :map doom-leader-map
  "j" #'avy-goto-char))

;; get rid of some annoying bindings
(map!
 ;; get rid of the annoying mouse-wheel text-scaling
 ;; which interacts badly with my screen-zoom
 "C-<wheel-down>" nil
 "C-<wheel-up>" nil

 ;; get rid of C-z suspend-frame which i keep on hitting
 ;; when going for C-` +popup/toggle
 "C-z" nil)

(map!
 ;; don't show the messages buffer on minibuffer click
 (:map minibuffer-inactive-mode-map
  "<mouse-1>" #'ignore))

;; doom has its own auto-revert stuff, so
;; this shouldn't be necessary... but i observed some
;; files not being auto-reverted when i thought they should be
;; (setq auto-revert-use-notify t)
;; (global-auto-revert-mode t)


