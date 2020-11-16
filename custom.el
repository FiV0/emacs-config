;;; custom-config --- Customization mostly set programmtically
;;;
;;; Commentary:
;;; You should not edit this file by hand, except for allowing
;;; custom .dir-locals.el variables to be set permanently.
;;;
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("837f2d1e6038d05f29bbcc0dc39dbbc51e5c9a079e8ecd3b6ef09fc0b149ceb1" default)))
 '(package-selected-packages
   (quote
    (use-package slime-company slime rainbow-delimiters powerline paredit lispyville ido-completing-read+ highlight-parentheses helm-projectile helm-ag flycheck-clj-kondo expand-region evil-surround evil-nerd-commenter evil-magit evil-leader evil company-quickhelp company-lsp company cider-eval-sexp-fu cider camcorder bind-key aggressive-indent ace-jump-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 113 :width normal)))))

(provide 'custom)
;;; custom.el ends here
