;;; init-package --- automatically loads your packages
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("melpa"        . 5)
        ("gnu"          . 0)))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)


;; list the packages you want
(setq package-list
      '(aggressive-indent
        avy
        bind-key
        camcorder
        ;; cider
        cider-eval-sexp-fu
        company
        company-quickhelp
        company-terraform
        dash
        dap-mode
        eval-sexp-fu
        evil
        evil-collection
        evil-leader
        evil-nerd-commenter
        evil-surround
        expand-region
        flycheck-clj-kondo
        groovy-mode
        helm-ag
        helm-projectile
        highlight-parentheses
        ido-completing-read+
        lispyville
        lsp-java
        lsp-mode
        lsp-ui
        magit
        markdown-mode
        monokai-theme
        paredit
        poly-ansible
        powerline
        projectile
        rainbow-delimiters
        smartparens
        slime
        slime-company
        terraform-mode
        undo-tree
        use-package
        yaml-mode))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-packages)
;;; init-packages.el ends here
