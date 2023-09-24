;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
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
(setq doom-theme 'doom-monokai-classic)
(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 15))
(setq doom-themes-enable-italic nil)

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

(defun add-user-dir-path (path)
  (concat user-emacs-directory path))

;; this saves customization to an extra file and doesn't mess
;; with your init.el
(setq custom-file (add-user-dir-path "custom.el"))
(load custom-file :noerror)

;; maximize the window
(setq initial-frame-alist '((fullscreen . maximized)))

;; this is a customized so that the config works on large screens
;; as well as a laptop with one vertical split
(setq split-width-threshold 200)
(setq split-height-threshold nil)

(after! aggressive-indent
  (add-to-list 'aggressive-indent-excluded-modes
               'cider-repl-mode)
  (add-to-list 'aggressive-indent-excluded-modes
               'elisp-mode))

(after! cider
  ;; kill repl without asking
  (setq confirm-kill-processes nil)

  (setq cider-prompt-for-symbol nil)
  (setq cider-save-file-on-load t)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 1000)
  (setq cider-repl-history-file "~/.cider-repl-history")
  (setq cider-auto-jump-to-error 'errors-only)

  ;; consider forms in comment forms top level
  (setq clojure-toplevel-inside-comment-form t)

  ;; prevent long eval times the first time a cljs form is evaled
  (setq cider-auto-track-ns-form-changes nil)

  ;; repl popup behaviour
  (set-popup-rules!
    '(("^\\*cider-repl"
       :side right
       :modeline t
       :width 0.5
       :quit nil
       :ttl) nil)))

(after! clojure-mode
  (define-clojure-indent
    (as-> 1)
    (match 1)))

(after! company
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (map! :map company-active-map "TAB" #'company-complete-common-or-cycle))

(after! expand-region
  (global-set-key (kbd "s-e") 'er/expand-region)
  (global-set-key (kbd "s-c") 'er/contract-region))

(after! smartparens
  (global-set-key (kbd "M-w") 'sp-forward-slurp-sexp)
  (global-set-key (kbd "M-b") 'sp-forward-barf-sexp))

(after! free-keys
  (bind-key "C-h C-k" 'free-keys))

(after! evil-surround
  (add-to-list 'evil-surround-operator-alist
               '(evil-paredit-change . change))
  (add-to-list 'evil-surround-operator-alist
               '(evil-paredit-delete . delete)))

(after! evil-nerd-commenter
  (map! :localleader
        "ci" 'evilnc-comment-or-uncomment-lines
        "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
        "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
        "cc" 'evilnc-copy-and-comment-lines
        "cp" 'evilnc-comment-or-uncomment-paragraphs
        "cr" 'comment-or-uncomment-region
        "cv" 'evilnc-toggle-invert-comment-line-by-line
        "."  'evilnc-copy-and-comment-operator
        "\\" 'evilnc-comment-operator))
