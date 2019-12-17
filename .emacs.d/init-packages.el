(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))


; list the packages you want
(setq package-list
      '(aggressive-indent powerline evil-magit evil magit kotlin-mode
                          dap-mode lsp-java company-lsp lsp-mode elpy lispyville markdown-mode
                          company-quickhelp slime-company rainbow-delimiters
                          evil-nerd-commenter evil-leader use-package
                          highlight-parentheses cider bind-key tabbar
                          paredit company slime evil-surround))

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
