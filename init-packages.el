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
      '(ace-jump-mode
        aggressive-indent
        bind-key
        camcorder
        cider
        cider-eval-sexp-fu
        company
        company-lsp
        company-quickhelp
        dash
        eval-sexp-fu
        evil
        evil-leader
        evil-magit
        evil-nerd-commenter
        evil-surround
        expand-region
        flycheck-clj-kondo
        helm-ag
        helm-projectile
        highlight-parentheses
        ido-completing-read+
        lispyville
        lsp-mode
        magit
        markdown-mode
        paredit
        powerline
        projectile
        rainbow-delimiters
        slime
        slime-company
        use-package))


;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-packages)
;;; init-packages.el ends here
