;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(load "~/.emacs.d/init-packages.el")

;; custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(company-backends
   (quote
    (company-slime company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                   (company-dabbrev-code company-gtags company-etags company-keywords)
                   company-oddmuse company-dabbrev)))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("837f2d1e6038d05f29bbcc0dc39dbbc51e5c9a079e8ecd3b6ef09fc0b149ceb1" "2925ed246fb757da0e8784ecf03b9523bccd8b7996464e587b081037e0e98001" "a21be90bf7f37922e647eb3c5b8fbaa250b3b0db9daee4dbf510863a4f9006a4" default)))
 '(elpy-syntax-check-command "flake8")
 '(global-display-line-numbers-mode t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (camcorder aggressive-indent powerline evil-magit evil magit kotlin-mode dap-mode lsp-java company-lsp lsp-mode elpy lispyville markdown-mode company-quickhelp slime-company rainbow-delimiters evil-nerd-commenter evil-leader use-package cider bind-key tabbar paredit company slime evil-surround)))
 ;; '(safe-local-variable-values
 ;;   (quote
 ;;    ((cider-shadow-cljs-default-options . "app")
 ;;     (cider-default-cljs-repl . shadow))
 ;;    ))
 '(show-paren-mode t)
 '(tabbar-background-color "gray20")
 '(tabbar-separator (quote (0.5)))
 '(tabbar-use-images nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 113 :width normal)))))

;; general EMACS stuff
(require 'use-package)
(setq split-width-threshold 200)
(setq split-height-threshold nil)
(setq inhibit-splash-screen t)

;; save auto-save and backup files somewhere else
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; global linenumbers
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; dired directory sorting
(setq dired-listing-switches "-alX  --group-directories-first")

;; tabs as spaces
(progn (setq-default indent-tabs-mode nil))

;; matching parentheses
(show-paren-mode 1)

;; set cursor color to red
(set-cursor-color "#ef330e")

;; Highlighting of FIXME and TODO
;; (require 'fic-mode)
;; (setq fic-highlighted-words '("FIXME" "TODO"))
;; (add-hook 'lisp-mode-hook #'turn-on-fic-mode)
;; (add-hook 'clojure-mode-hook #'turn-on-fic-mode)
;; (fic-mode 1)

;; Evil mode
(setq evil-want-C-u-scroll t)
;; (setq evil-want-keybinding nil)
(require 'evil)
;;allow tabs in evil mode
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(evil-mode 1)

;;magit
(require 'evil-magit)

;; leader mode
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
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
  ;; to be consistent with slime
  "q" 'cider-popup-buffer-quit-function)

;; evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; compatibility with paredit
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-change . change))
(add-to-list 'evil-surround-operator-alist
             '(evil-paredit-delete . delete))

;; company related things
;; company-mode in all buffers
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key "\t" 'company-complete-common-or-cycle)
(company-tng-configure-default)

;; start completions immediately
(setq company-idle-delay 0)
(setq company-selection-wrap-around t)

;; company quickhelp
(company-quickhelp-mode)

;; for lsp-mode
;; (require 'company-lsp)
;; (push 'company-lsp company-backends)

;;java stuff
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

;; paredit hooks
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)
;; lispyville for balancing parentheses and quotes
(add-hook 'emacs-lisp-mode-hook       #'lispyville-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'lispyville-mode)
(add-hook 'ielm-mode-hook             #'lispyville-mode)
(add-hook 'lisp-mode-hook             #'lispyville-mode)
(add-hook 'lisp-interaction-mode-hook #'lispyville-mode)
(add-hook 'scheme-mode-hook           #'lispyville-mode)
(add-hook 'clojure-mode-hook          #'lispyville-mode)
(add-hook 'cider-repl-mode-hook       #'lispyville-mode)
(add-hook 'slime-repl-mode-hook       #'lispyville-mode)
;; rainbow parentheses
(add-hook 'emacs-lisp-mode-hook       #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook             #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook             #'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook           #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook          #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook       #'rainbow-delimiters-mode)
(add-hook 'slime-repl-mode-hook       #'rainbow-delimiters-mode)
;; aggressive indent
(global-aggressive-indent-mode 1)
;; don't indent comments in SLIME repl and cider-repl
(add-to-list 'aggressive-indent-excluded-modes
             'slime-repl-mode)
(add-to-list 'aggressive-indent-excluded-modes
             'cider-repl-mode)
;; (add-to-list 'aggressive-indent-dont-indent-if
;;              '(and (derived-mode-p 'slime-repl-mode)
;;                    (looking-back "; *")))

;; make normal mode the default
(setq evil-emacs-state-modes nil)

;; new empty buffer without prompting for a name
(defun new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (funcall initial-major-mode)
    (switch-to-buffer $buf)
    (setq buffer-offer-save t)
    $buf))

;; slime stuff
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-quicklisp slime-asdf slime-company))
(slime-setup '(slime-fancy slime-company))
(setq slime-company-completion 'fuzzy)
;; avoid slime-company overriding the above grouped backends
;; the slime-company contrib pushs slime-company as single backend to company-backends
;; (defun slime-avoid-override ()
;;   (pop company-backends)
;;   (push '(company-slime company-dabbrev) company-backends))
;;(add-hook 'slime-connected-hook 'slime-avoid-override)

(defun sldb-leader-setup ()
  (evil-leader/set-key
    "a" 'sldb-abort))

(add-hook 'sldb-hook 'sldb-leader-setup)

;; cider stuff
(setq cider-prompt-for-symbol nil)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.cider-repl-history")
(setq cider-auto-jump-to-error 'errors-only)
;; (setq cider-preferred-build-tool 'lein)

(defun my-cider-jack-in ()
  (interactive)
  (cider-jack-in-clj '(4)))
(add-hook 'clojure-mode-hook
          (lambda () (local-set-key (kbd "C-c C-j") #'my-cider-jack-in)))

(defun my-cider-mode-override ()
  ;; cider overwrite evil
  (define-key evil-normal-state-map (kbd "M-.") nil)
  ;; custom debug command
  (define-key cider-mode-map (kbd "C-c C-y") #'cider-debug-defun-at-point)
  (setq cider-repl-display-help-banner nil))

;; cider evil interpolation
;; (add-hook 'clojure-mode-hook          #'my-cider-mode-override)
(add-hook 'cider-repl-mode-hook       #'my-cider-mode-override)

;; cider + evil interpolation
(defun my-cider-debug-setup ()
  (evil-make-overriding-map cider--debug-mode-map 'normal)
  (evil-normalize-keymaps))
(add-hook 'cider--debug-mode-hook 'my-cider-debug-setup)

;; elpy stuff
(package-initialize)
(elpy-enable)

;; My preferred keys
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<down>") 'kill-this-buffer) ;; this overrides some paredit binding
    (define-key map (kbd "M-<up>") 'new-empty-buffer)
    (define-key map (kbd "<M-left>") 'tabbar-backward-tab)
    (define-key map (kbd "<M-right>") 'tabbar-forward-tab)
    (define-key map (kbd "M-h") 'tabbar-backward-group)
    (define-key map (kbd "M-l") 'tabbar-forward-group)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys-minor-mode")

(my-keys-minor-mode 1)

(add-hook 'after-load-functions 'my-keys-have-priority)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)


;; tabbar stuff
(require 'tabbar)
(tabbar-mode 1)
(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
  "Returns the name of the tab group names the current buffer belongs to.
    There are two groups: Emacs buffers (those whose name starts with '*', plus
    dired buffers), and the rest.  This works at least with Emacs v24.2 using
    tabbar.el v1.7."
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
        ((eq major-mode 'dired-mode) "emacs")
        (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; (setq *tabbar-ignore-buffers* '("*scratch*" "*Messages*" "*GNU Emacs*"
;;                                 "*inferior-lisp*" "*slime-events*"))
;;(setq *tabbar-ignore-buffers* '())
;; (defun remove-unwanted-buffers ()
;;   (buffer-list))

;; (setq tabbar-buffer-list-function 'remove-unwanted-buffers)

;; (setq tabbar-buffer-list-function
;;       (lambda ()
;;         (buffer-list)
;;         (remove-if
;;                   (lambda (buffer) nil
;;                     (and (not (eq (current-buffer) buffer)) ; Always include the current buffer.
;;                          (loop for name in *tabbar-ignore-buffers* ;remove buffer name in this list.
;;                                thereis (string-equal (buffer-name buffer) name))
;;                          )
;;                     )
;;                   (buffer-list)
;;                   )
;;         ))

;; Colors for tabbar
(set-face-attribute 'tabbar-default nil
                    :background "gray20" :foreground
                    "gray60" :distant-foreground "gray50"
                    :family "DejaVu Sans Mono" :box nil)
(set-face-attribute 'tabbar-unselected nil
                    :background "gray80" :foreground "black" :box nil)
(set-face-attribute 'tabbar-modified nil
                    :foreground "red4" :box nil
                    :inherit 'tabbar-unselected)
(set-face-attribute 'tabbar-selected nil
                    :background "#4090c0" :foreground "white" :box nil)
(set-face-attribute 'tabbar-selected-modified nil
                    :inherit 'tabbar-selected :foreground "GoldenRod2" :box nil)
(set-face-attribute 'tabbar-button nil
                    :box nil)

;; Use Powerline to make tabs look nicer
;; this needs to run *after* the colors are set
(require 'powerline)
(defvar my/tabbar-height 20)
(defvar my/tabbar-left (powerline-wave-right 'tabbar-default nil my/tabbar-height))
(defvar my/tabbar-right (powerline-wave-left nil 'tabbar-default my/tabbar-height))
(defun my/tabbar-tab-label-function (tab)
  (powerline-render (list my/tabbar-left
        (format " %s  " (car tab))
        my/tabbar-right)))
(setq tabbar-tab-label-function #'my/tabbar-tab-label-function)
