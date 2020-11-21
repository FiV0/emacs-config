;;; emacs-config --- entry point to my emacs config
;;; Commentary:
;;; Code:
(defun add-user-dir-path (path)
  (concat user-emacs-directory path))

;; this saves customization to an extra file and doesn't mess
;; with your init.el
(setq custom-file (add-user-dir-path "custom.el"))
(load custom-file :noerror)

;; initialized and installs (if necessary) the packages
(load (add-user-dir-path "init-packages.el"))

;;; General emacs stuff

;; custom themes
(add-to-list 'custom-theme-load-path (add-user-dir-path "themes/"))
(setq custom-enabled-themes '(monokai))
(load-theme 'monokai)

;; maximize window when opening emacs
(setq initial-frame-alist '((fullscreen . maximized)))

;; always show linenumbers
(global-display-line-numbers-mode t)

;; always show matching pairs of characters
(show-paren-mode t)

;; set cursor color to red
(set-cursor-color "#ef330e")

;; this is a customized so that the config works
;; on large screens as well as a laptop with
;; one vertical split
(setq split-width-threshold 200)
(setq split-height-threshold nil)

;; don't show the splash screen
(setq inhibit-splash-screen t)

;; remove trailing whitespaces whenever a file is written
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; do line breaks instead of scrolling
(put 'scroll-left 'disabled nil)

(load "server") ;; TODO check if this is needed
(unless (server-running-p) (server-start))

;; save auto-save and backup files somewhere else
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; dired directory sorting
(setq dired-listing-switches "-alX  --group-directories-first")

;; tabs as spaces
(progn (setq-default indent-tabs-mode nil))

;; spellchecking
(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'emacs-lisp-mode-hook       'flyspell-prog-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'flyspell-prog-mode)
(add-hook 'ielm-mode-hook             'flyspell-prog-mode)
(add-hook 'lisp-mode-hook             'flyspell-prog-mode)
(add-hook 'lisp-interaction-mode-hook 'flyspell-prog-mode)
(add-hook 'scheme-mode-hook           'flyspell-prog-mode)

(setq flyspell-issue-message-flag nil)

;; lisp indent
(setq lisp-indent-offset nil)


;; evil mode
(setq evil-want-C-u-scroll t) ;important to appear before the requrie
(require 'evil)
;;allow tabs in evil mode
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(evil-mode 1)
;; make normal mode the default
(setq evil-emacs-state-modes nil)

;;magit
(require 'evil-magit)
(setq magit-save-repository-buffers 'dontask)


;; projectile
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-indexing-method 'hybrid) ;; indexes non tracked files

(require 'helm-projectile)
(helm-projectile-on)

;; ido
(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode 1)
(ido-everywhere 1)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; ace-jump-mode
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; expand-region
(global-set-key (kbd "s-e") 'er/expand-region)
(global-set-key (kbd "s-c") 'er/contract-region)

;; evil-leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; nerd-commenter
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "."  'evilnc-copy-and-comment-operator
  "\\" 'evilnc-comment-operator) ; if you prefer backslash key

(require 'evil-surround)
(global-evil-surround-mode 1)

;; compatibility with paredit
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-change . change))
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-delete . delete))

;; flycheck
(global-flycheck-mode)
;; so flycheck knows about emacs packages when editing
;; config files
(setq-default flycheck-emacs-lisp-load-path 'inherit)


(require 'company)
;; enable company globally
(add-hook 'after-init-hook 'global-company-mode)
;; use tab to cycle completions and immediately enter them
;; into the buffer
(global-set-key "\t" 'company-complete-common-or-cycle)
(company-tng-mode)

;; start completions immediately
(setq company-idle-delay 0)
(setq company-selection-wrap-around t)

(setq company-backends
      ;; XXX the order is important here
      '(company-slime
        company-bbdb
        company-eclim
        company-semantic
        company-clang
        company-xcode
        company-cmake
        company-capf
        company-files
        (company-dabbrev-code company-gtags company-etags company-keywords)
        company-oddmuse
        company-dabbrev))

;; company quickhelp
(company-quickhelp-mode)

;; aggressive indent
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes
             'elisp-mode)

;; TODO if scheme gets its own file move the hooks over there

;; paredit hooks
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; lispyville for balancing parentheses and quotes
(add-hook 'emacs-lisp-mode-hook       #'lispyville-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'lispyville-mode)
(add-hook 'ielm-mode-hook             #'lispyville-mode)
(add-hook 'lisp-mode-hook             #'lispyville-mode)
(add-hook 'lisp-interaction-mode-hook #'lispyville-mode)
(add-hook 'scheme-mode-hook           #'lispyville-mode)

;; rainbow parentheses
(add-hook 'emacs-lisp-mode-hook       #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook             #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook             #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook           #'rainbow-delimiters-mode)

;; new empty buffer without prompting for a name
(defun new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (setq buffer-offer-save t)
    $buf))

;; My preferred keys
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<down>") 'kill-this-buffer) ;; this overrides some paredit binding
    (define-key map (kbd "M-<up>") 'new-empty-buffer)
    (define-key map (kbd "<M-left>") 'awesome-tab-backward-tab)
    (define-key map (kbd "<M-right>") 'awesome-tab-forward-tab)
    (define-key map (kbd "M-h") 'awesome-tab-backward-group)
    (define-key map (kbd "M-l") 'awesome-tab-forward-group)
    ;; this is is better to use when not on keyboard.io
    (define-key map (kbd "M-s-<left>") 'awesome-tab-backward-group)
    (define-key map (kbd "M-s-<right>") 'awesome-tab-forward-group)
    (define-key map (kbd "C-<left>") 'awesome-tab-move-current-tab-left)
    (define-key map (kbd "C-<right>") 'awesome-tab-move-current-tab-to-right)
    map)
  "My-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys-minor-mode")

(my-keys-minor-mode 1)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(add-hook 'after-load-functions 'my-keys-have-priority)

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook
          'my-minibuffer-setup-hook)

;; custom files
(load (add-user-dir-path "custom-cl.el")) ; common lisp
;; it's important that this file gets loaded after
;; the evil settings as it otherwise messes up
;; the C-u binding
(load (add-user-dir-path "custom-clj.el")) ; clojure
;; awesome is a local package
(add-to-list 'load-path (add-user-dir-path "awesome-tab/"))
(load (add-user-dir-path "custom-awesome-tab.el")) ; awesome-tab
