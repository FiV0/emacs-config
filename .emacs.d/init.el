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
    (cider-eval-sexp-fu ido-completing-read+ helm-ag dash expand-region ace-jump-mode flycheck-clj-kondo helm-projectile projectile camcorder aggressive-indent powerline evil-magit evil magit dap-mode company-lsp lsp-mode lispyville markdown-mode company-quickhelp slime-company rainbow-delimiters evil-nerd-commenter evil-leader use-package cider bind-key tabbar paredit company slime evil-surround)))
 '(safe-local-variable-values
   (quote
    ((cider-clojure-cli-global-options . "-A:global:dev:test:tn5250j")
     (eval define-clojure-indent
           (assoc 0)
           (ex-info 0)
           (for! 1)
           (for* 1)
           (as-> 2)
           (uix/context-provider 1)
           (nextjournal\.commands\.api/register! 1)
           (nextjournal\.commands\.api/register-context-fn! 1)
           (commands/register! 1))
     (cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")
     (cider-enhanced-cljs-completion-p)
     (cider-clojure-cli-global-options . "-A:dev:test")
     (eval
      (lambda nil
        (when
            (not
             (featurep
              (quote nextjournal)))
          (let
              ((init-file-path
                (expand-file-name "emacs.d/nextjournal.el" default-directory)))
            (when
                (file-exists-p init-file-path)
              (load init-file-path)
              (require
               (quote nextjournal)))))))
     (eval define-clojure-indent
           (assoc 0)
           (ex-info 0)
           (for! 1)
           (for* 1)
           (as-> 2)
           (uix/context-provider 1)
           (ductile\.ui\.commands\.api/register! 1)
           (ductile\.ui\.commands\.api/register-context-fn! 1)
           (commands/register! 1))
     (web-mode-markup-indent-offset . default-indent)
     (web-mode-css-indent-offset . default-indent)
     (web-mode-code-indent-offset . default-indent)
     (javascript-indent-level . default-indent)
     (css-indent-offset . default-indent)
     (default-indent . 2)
     (js2-mode-show-strict-warnings)
     (magit-save-repository-buffers . dontask)
     (frame-resize-pixelwise . t)
     (display-line-numbers-width-start . t)
     (cljr-magic-requires)
     (cider-save-file-on-load)
     (cider-repl-display-help-banner)
     (cider-redirect-server-output-to-repl . t)
     (clojure-toplevel-inside-comment-form . t)
     (cider-auto-track-ns-form-changes)
     (cider-clojure-cli-global-options . "-A:global:dev:test")
     (eval
      (lambda nil
        (let
            ((init-file-path
              (expand-file-name "emacs.d/nextjournal.el" default-directory)))
          (when
              (file-exists-p init-file-path)
            (load init-file-path)
            (require
             (quote nextjournal))))))
     (eval
      (lambda nil
        (let
            ((init-file-path
              (expand-file-name "spacemacs.d/nextjournal.el" default-directory)))
          (when
              (file-exists-p init-file-path)
            (load init-file-path)
            (require
             (quote nextjournal))))))
     (eval progn
           (defadvice cider--choose-reusable-repl-buffer
               (around auto-confirm compile activate)
             (cl-letf
                 (((symbol-function
                    (quote y-or-n-p))
                   (lambda
                     (&rest args)
                     t))
                  ((symbol-function
                    (quote completing-read))
                   (lambda
                     (prompt collection &rest args)
                     (car collection))))
               ad-do-it)))
     (cider-refresh-after-fn . "com.nextjournal.journal.repl/post-refresh")
     (cider-refresh-before-fn . "com.nextjournal.journal.repl/pre-refresh")
     (cider-preferred-build-tool . clojure-cli)
     (eval progn
           (make-variable-buffer-local
            (quote cider-jack-in-nrepl-middlewares))
           (add-to-list
            (quote cider-jack-in-nrepl-middlewares)
            "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-custom-cljs-repl-init-form . "(do (user/cljs-repl) (user/browse 8280))")
     (cider-default-cljs-repl . custom)
     (cider-clojure-cli-global-options . "-A:dev")
     (cider-shadow-default-options . ":app")
     (cider-default-cljs-repl . shadow)
     (cider-custom-cljs-repl-init-form . "(do (user/go) (user/cljs-connect) (user/browse 8280))")
     (cider-custom-cljs-repl-init-form . "(do (user/go) (user/cljs-repl))")
     (cider-clojure-cli-global-options . "-A:global:dev:test:tn5250j"))))
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
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

(load "server")
(unless (server-running-p) (server-start))
;; (setq enable-dir-local-variables t)

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

;; enable goto globally
;; (goto-address-mode 1)

;; lisp indent
(setq lisp-indent-offset nil)

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
;; make normal mode the default
(setq evil-emacs-state-modes nil)

;;magit
(require 'evil-magit)
(setq magit-save-repository-buffers 'dontask)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-indexing-method 'hybrid) ;; indexes non tracked files

;; (setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)
(helm-projectile-on)

;; ido
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

;; set repl buffers to insert initially
(evil-set-initial-state 'cider-repl-mode 'insert)

;; aggressive indent
(global-aggressive-indent-mode 1)
;; don't indent comments in SLIME repl and cider-repl
(add-to-list 'aggressive-indent-excluded-modes
             'slime-repl-mode)
(add-to-list 'aggressive-indent-excluded-modes
             'cider-repl-mode)
(add-to-list 'aggressive-indent-excluded-modes
             'elisp-mode)

;; (add-to-list 'aggressive-indent-dont-indent-if
;;              '(and (derived-mode-p 'slime-repl-mode)
;;                    (looking-back "; *")))


;; new empty buffer without prompting for a name
(defun new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    ;; (funcall initial-major-mode)
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
(setq nrepl-prompt-to-kill-server-buffer-on-quit nil)

(with-eval-after-load 'clojure-mode
  (define-clojure-indent
    (as-> 1)
    (match 1)))

;; consider forms in comment forms top level
(setq clojure-toplevel-inside-comment-form t)

;; prevent long eval times the first time a cljs form is evaled
(setq cider-auto-track-ns-form-changes nil)

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

  (define-key cider-mode-map (kbd "C-c C-p") #'cider-pprint-eval-last-sexp-to-comment)

  (setq cider-repl-display-help-banner nil)
  (evil-leader/set-key
    "cb" 'cider-repl-clear-buffer))

;; copied from lesser-evil
(defun cider-pprint-register (register)
  (interactive (list (register-read-with-preview "Eval register: ")))
  (cider--pprint-eval-form (get-register register)))

(evil-leader/set-key
  "," 'cider-pprint-register)

(set-register ?r "(user/reset)")

;; cider evil interpolation
;; (add-hook 'clojure-mode-hook          #'my-cider-mode-override)
(add-hook 'cider-repl-mode-hook       #'my-cider-mode-override)


;; cider + evil interpolation
(defun my-cider-debug-setup ()
  (evil-make-overriding-map cider--debug-mode-map 'normal)
  (evil-normalize-keymaps))
(add-hook 'cider--debug-mode-hook 'my-cider-debug-setup)

;; cider cljs safe .dir-locals
(add-to-list 'safe-local-variable-values '(cider-default-cljs-repl . shadow))
(add-to-list 'safe-local-variable-values '(cider-shadow-default-options . ":app"))

;; clj-kondo
(global-flycheck-mode)
(require 'flycheck-clj-kondo)

(add-hook 'cider-mode-hook
          (lambda () (setq next-error-function #'flycheck-next-error-function)))

;; load CIDER from its source code
;; (add-to-list 'load-path "~/Code/Clojure/cider/")
(require 'cider-eval-sexp-fu)

;; (load "cider-autoloads" t t)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/awesome-tab/"))

(require 'awesome-tab)

(awesome-tab-mode t)
(awesome-tab-build-helm-source)

(setq *awesome-tab-ignore-buffers* '("*scratch*"  "*GNU Emacs*"
                                     "*inferior-lisp*" "*slime-events*"))
(setq awesome-tab-height 120)
(setq awesome-tab-dark-unselected-blend 0.7)
(setq awesome-tab-dark-active-bar-color "#F62459")
(setq awesome-tab-active-bar-width 5)
(setq awesome-tab-show-tab-index t)

(global-set-key (kbd "s-1") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-2") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-3") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-4") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-5") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-6") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-7") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-8") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-9") 'awesome-tab-select-visible-tab)
(global-set-key (kbd "s-0") 'awesome-tab-select-visible-tab)

(defun awesome-tab-hide-tab (x)
  (let ((name (format "%s" x)))
    (or (member name *awesome-tab-ignore-buffers*)
        (string-prefix-p "*helm" name))))

(defun awesome-tab-buffer-groups ()
  "`awesome-tab-buffer-groups' control buffers' group rules.
Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `awesome-tab-get-group-name' with project name."
  (list
   (let ((name (buffer-name)))
     (cond ((and (>= (length name) 6)
                 (or (string-equal "*slime" (substring name 0 6))
                     (string-equal "*cider" (substring name 0 6))
                     (string-equal "*ielm" (substring name 0 5))))
            "repl")
           ((string-equal "*" (substring name 0 1))
            "emacs")
           ((eq major-mode 'dired-mode)
            "dired")
           ((string-equal "magit" (substring name 0 5))
            "magit")
           (t "user")))))

(global-set-key (kbd "M-s-1") (lambda () (interactive) (awesome-tab-switch-group "user")))
(global-set-key (kbd "M-s-2") (lambda () (interactive) (awesome-tab-switch-group "repl")))
(global-set-key (kbd "M-s-3") (lambda () (interactive) (awesome-tab-switch-group "emacs")))
(global-set-key (kbd "M-s-4") (lambda () (interactive) (awesome-tab-switch-group "dired")))
(global-set-key (kbd "M-s-5") (lambda () (interactive) (awesome-tab-switch-group "magit")))

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

(add-hook 'minibuffer-setup-hook
          'my-minibuffer-setup-hook)

(put 'scroll-left 'disabled nil)
