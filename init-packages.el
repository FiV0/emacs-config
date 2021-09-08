;;; init-package --- automatically loads your packages
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

;; activate all the packages
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))


;; list the packages you want
(setq package-list
      '(adoc-mode
        aggressive-indent
        avy
        bind-key
        camcorder
        cider
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
        exec-path-from-shell
        expand-region
        flycheck
        flycheck-clj-kondo
        free-keys
        gnu-elpa-keyring-update
        groovy-mode
        helm-ag
        helm-projectile
        highlight-parentheses
        hl-todo
        ido-completing-read+
        kotlin-mode
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
        rustic
        smartparens
        slime
        slime-company
        terraform-mode
        use-package
        which-key
        yaml-mode
        yasnippet))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-packages)
;;; init-packages.el ends here
