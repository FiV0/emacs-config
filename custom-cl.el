;;; custom-cl --- custom configuration for Common Lisp
;;; Commentary:
;;; Code:

(require 'slime)
(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy slime-quicklisp slime-asdf slime-company))
(slime-setup '(slime-fancy slime-company))

;;paredit
(add-hook 'slime-repl-mode-hook       #'enable-paredit-mode)

;; lispyville
(add-hook 'slime-repl-mode-hook       #'lispyville-mode)

;; rainbow-parentheses
(add-hook 'slime-repl-mode-hook       #'rainbow-delimiters-mode)


(require 'slime-company)
(setq slime-company-completion 'fuzzy)

(require 'aggressive-indent)
;; don't indent comments in SLIME repl
(add-to-list 'aggressive-indent-excluded-modes
             'slime-repl-mode)

;; TODO check if this is still needed
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


(provide 'custom-cl)
;;; custom-cl.el ends here
