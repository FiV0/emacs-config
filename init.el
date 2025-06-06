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
(load-theme 'monokai t)

;; maximize window when opening emacs
(setq initial-frame-alist '((fullscreen . maximized)))

;; always show linenumbers
(global-display-line-numbers-mode t)

;; disable scroll bar and tool-bar
(toggle-scroll-bar -1)
(tool-bar-mode -1)

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

(require 'adoc-mode)

;; spellchecking
(require 'flyspell)
(add-hook 'text-mode-hook                        'flyspell-mode)
(add-hook 'emacs-lisp-mode-hook                  'flyspell-prog-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'flyspell-prog-mode)
(add-hook 'ielm-mode-hook                        'flyspell-prog-mode)
(add-hook 'lisp-mode-hook                        'flyspell-prog-mode)
(add-hook 'lisp-interaction-mode-hook            'flyspell-prog-mode)
(add-hook 'scheme-mode-hook                      'flyspell-prog-mode)
;; so strings are not spellchecked in prog-mod
(setq flyspell-prog-text-faces (delq 'font-lock-string-face flyspell-prog-text-faces))

(setq flyspell-issue-message-flag nil)

;; lisp indent
(setq lisp-indent-offset nil)

;; evil mode
(setq evil-want-C-u-scroll t) ;important to appear before the requrie
(setq evil-want-keybinding nil)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
;;allow tabs in evil mode
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(evil-mode 1)
;; make normal mode the default
(setq evil-emacs-state-modes nil)
(evil-set-undo-system 'undo-redo)

;;magit is transitive dependency via evil-collection
(setq magit-save-repository-buffers 'dontask)

(global-set-key (kbd "M-o") 'occur)

;; projectile
(require 'projectile)
(projectile-global-mode)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-indexing-method 'hybrid) ;; indexes non tracked files
;;(setq projectile-indexing-method 'alien) ;; indexes non tracked files
(setq projectile-sort-order 'recently-active)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)

;; to disable weird helm completion error
;; see non defined function here
;; https://github.com/emacs-helm/helm/blob/72c61b2d0cb3cd48fb1b24d7708ad1794eeeb10c/helm.el#L4359
(require 'helm-mode)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq helm-split-window-default-side 'right)
;; depending on the project set this to nil or not
(setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(helm-projectile-on)
;; resuming helm session
(global-set-key (kbd "s-r") 'helm-resume)

;; ido
(require 'ido)
(setq ido-enable-flex-matching t)
(ido-mode 1)
(ido-everywhere 1)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; ace-jump-mode
(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-word-or-subword-1)
(define-key evil-normal-state-map (kbd "C-x SPC") 'avy-pop-mark)

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

;; yasnippet
;; (require 'yasnippet)
;; (setq yas-snippet-dirs
;;       (list (expand-file-name "snippets" user-emacs-directory))) ;; personal snippets
;; (yas-global-mode 1)

;; hl-todo
(global-hl-todo-mode)
(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("HACK"   . "#FF0000")
        ("DEBUG"  . "#A020F0")))

;; which-key
(setq which-key-idle-delay 0.5)
(which-key-mode)

;; javascript
(setq js-indent-level 2)

;; java
;; (require 'lsp-mode)
;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)
;; (setq lsp-java-java-path (replace-regexp-in-string "\n$" "" (shell-command-to-string "which java")))
;; (setq lsp-java-configuration-runtimes '[(:name "JavaSE-1.8"
;;                                                :path "/usr/lib/jvm/java-8-openjdk-amd64"
;;                                                :default t)])

(with-eval-after-load 'lsp-mode
  (global-set-key (kbd "C-c a") 'lsp-execute-code-action))

(require 'dap-mode)
;; (dap-auto-configure-mode)
(with-eval-after-load 'dap-mode
  (defun dap-go-to-output-buffer (&optional no-select)
    "Go to output buffer."
    (interactive)
    (display-buffer (dap--debug-session-output-buffer (dap--cur-session-or-die)))))

(require 'lsp-ui)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-sideline-actions-icon nil)
(setq lsp-enable-indentation nil)
(setq lsp-enable-file-watchers t)
(setq lsp-file-watch-threshold 2000)

(require 'company)
;; enable company globally
(add-hook 'after-init-hook 'global-company-mode)
;; use tab to cycle completions and immediately enter them
;; into the buffer
(global-set-key "\t" 'company-complete-common-or-cycle)
(setq company-minimum-prefix-length 2)
(company-tng-mode)

;; start completions immediately
(setq company-idle-delay 0)
(setq company-selection-wrap-around t)

(setq company-backends
      ;; XXX the order is important here
      '(company-slime
        company-bbdb
        company-semantic
        company-clang
        company-cmake
        company-capf
        company-files
        (company-dabbrev-code company-gtags company-etags company-keywords)
        company-oddmuse
        company-dabbrev))

;; free-keys
(bind-key "C-h C-k" 'free-keys)

;; imenu
(bind-key "C-c i" 'imenu)

;; univeral-argument
(bind-key "M-u" 'universal-argument)

;; company quickhelp
(company-quickhelp-mode)

;; terraform
(require 'company-terraform)
(company-terraform-init)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; polymode
(require 'polymode)
(require 'poly-ansible)
(add-to-list 'auto-mode-alist '(".*\\.ya?ml\.j2\\'" . poly-ansible-mode))

;; aggressive indent
(require 'aggressive-indent)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes
             'elisp-mode)

;; smartparens
(require 'smartparens-config)
(global-set-key (kbd "M-w") 'sp-forward-slurp-sexp)
(global-set-key (kbd "M-b") 'sp-forward-barf-sexp)
(add-hook 'emacs-lisp-mode-hook                  #'smartparens-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'smartparens-mode)
(add-hook 'ielm-mode-hook                        #'smartparens-mode)
(add-hook 'lisp-mode-hook                        #'smartparens-mode)
(add-hook 'lisp-interaction-mode-hook            #'smartparens-mode)
(add-hook 'scheme-mode-hook                      #'smartparens-mode)
(add-hook 'java-mode-hook                        #'smartparens-mode)

;; TODO if scheme gets its own file move the hooks over there

;; paredit hooks
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

;; lispyville for balancing parentheses and quotes
(add-hook 'emacs-lisp-mode-hook                  #'lispyville-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'lispyville-mode)
(add-hook 'ielm-mode-hook                        #'lispyville-mode)
(add-hook 'lisp-mode-hook                        #'lispyville-mode)
(add-hook 'lisp-interaction-mode-hook            #'lispyville-mode)
(add-hook 'scheme-mode-hook                      #'lispyville-mode)

;; rainbow parentheses
(add-hook 'emacs-lisp-mode-hook                  #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook                        #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook                        #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook            #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook                      #'rainbow-delimiters-mode)

(require 'quelpa-use-package)

;; copilot is not in melpa
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :bind (("C-c M-f" . copilot-complete)
         :map copilot-completion-map
         ;; ("C-g" . 'copilot-clear-overlay)
         ;; ("M-p" . 'copilot-previous-completion)
         ;; ("M-n" . 'copilot-next-completion)
         ("<tab>" . 'copilot-accept-completion)
         ;; ("M-f" . 'copilot-accept-completion-by-word)
         ;; ("M-<return>" . 'copilot-accept-completion-by-line)
         )
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 4))
  (add-hook 'prog-mode-hook 'copilot-mode)
  (setq copilot-max-char 200000))

(require 'copilot)

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

(defun my-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; My preferred keys
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<down>") 'my-kill-this-buffer) ;; this overrides some paredit binding
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
