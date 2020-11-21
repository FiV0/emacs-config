;;; custom-clj --- custom configuration for Clojure
;;; Commentary:
;;; Code:

;; modify this to load CIDER from its source code
;; (add-to-list 'load-path "~/Code/Clojure/cider/")

(require 'cider)

;; cider stuff
(setq cider-prompt-for-symbol nil)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-history-file "~/.cider-repl-history")
(setq cider-auto-jump-to-error 'errors-only)
(setq cider-repl-display-help-banner nil)

(with-eval-after-load 'clojure-mode
  (define-clojure-indent
    (as-> 1)
    (match 1)))

;; consider forms in comment forms top level
(setq clojure-toplevel-inside-comment-form t)

;; prevent long eval times the first time a cljs form is evaled
(setq cider-auto-track-ns-form-changes nil)

;; copied from lesser-evil
(defun cider-pprint-register (register)
  (interactive (list (register-read-with-preview "Eval register: ")))
  (cider--pprint-eval-form (get-register register)))

(require 'evil)
(defun my-cider-mode-override ()
  ;; overwrite evil
  (define-key evil-normal-state-map (kbd "M-.") nil)
  ;; custom debug command
  (define-key cider-mode-map (kbd "C-c C-y") #'cider-debug-defun-at-point)
  ;; custom pprint to buffer command
  (define-key cider-mode-map (kbd "C-c C-p") #'cider-pprint-eval-last-sexp-to-comment)

  (evil-leader/set-key
    "cb" 'cider-repl-clear-buffer
    ;; to be consistent with slime
    "q" 'cider-popup-buffer-quit-function
    "," 'cider-pprint-register)

  (set-register ?r "(user/reset)")

  (setq confirm-kill-processes nil))

(defun my-cider-debug-setup ()
  (evil-make-overriding-map cider--debug-mode-map 'normal)
  (evil-normalize-keymaps))

;; cider + evil interpolation
(add-hook 'cider-repl-mode-hook       #'my-cider-mode-override)
(add-hook 'cider--debug-mode-hook     #'my-cider-debug-setup)

;; clj-kondo
(require 'flycheck-clj-kondo)
(add-hook 'cider-mode-hook
          (lambda () (setq next-error-function #'flycheck-next-error-function)))

;; paredit
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)

;; lispyville
(add-hook 'clojure-mode-hook          #'lispyville-mode)
(add-hook 'cider-repl-mode-hook       #'lispyville-mode)

;; rainbow-parentheses
(add-hook 'clojure-mode-hook          #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook       #'rainbow-delimiters-mode)

;; flyspell in prog-mode
(add-hook 'clojure-mode-hook          'flyspell-prog-mode)
(add-hook 'cider-repl-mode-hook       'flyspell-prog-mode)

(evil-leader/set-key
  ;; to be consistent with slime
  "q" 'cider-popup-buffer-quit-function)

(require 'aggressive-indent)
;; don't indent comments in a cider repl
(add-to-list 'aggressive-indent-excluded-modes
             'cider-repl-mode)

(require 'cider-eval-sexp-fu)

(provide 'custom-clj)
;;; custom-clj.el ends here
