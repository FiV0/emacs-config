;;; custom-clj --- custom configuration for Clojure
;;; Commentary:
;;; Code:

;; modify this to load CIDER from its source code
;; (add-to-list 'load-path "~/Code/Clojure/cider/")

(require 'cider)

;; cider stuff
(setq cider-prompt-for-symbol nil)
(setq cider-save-file-on-load t)
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
  ;; overrides some other cider keybinding
  (define-key cider-mode-map (kbd "C-c C-e") #'cider-pprint-eval-last-sexp)
  ;;
  (define-key cider-mode-map (kbd "M-.") #'cider-find-var)

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

;; smartparens
(add-hook 'clojure-mode-hook          #'smartparens-mode)
(add-hook 'cider-repl-mode-hook       #'smartparens-mode)

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

;; clojure lsp
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      lsp-headerline-breadcrumb-enable nil
      ;; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )


;; babashka
(defun babashka-quit ()
  (interactive)
  (when (get-buffer "*babashka-nrepl-server*")
    (kill-buffer "*babashka-nrepl-server*"))
  (when (get-buffer "*babashka-repl*")
    (kill-buffer "*babashka-repl*")))

(defun babashka-jack-in (&optional connected-callback)
  (interactive)
  (babashka-quit)
  (let* ((cmd "bb --nrepl-server 0")
         (serv-buf (get-buffer-create "*babashka-nrepl-server*"))
         (host "127.0.0.1")
         (repl-builder (lambda (port)
                         (lambda (_)
                           (let ((repl-buf (get-buffer-create "*babashka-repl*")))
                             (with-current-buffer repl-buf
                               (cider-repl-create (list :repl-buffer repl-buf
                                                        :repl-type 'clj
                                                        :host host
                                                        :port port
                                                        :project-dir "~"
                                                        :session-name "babashka"
                                                        :repl-init-function (lambda ()
                                                                              (setq-local cljr-suppress-no-project-warning t
                                                                                          cljr-suppress-middleware-warnings t)
                                                                              (rename-buffer "*babashka-repl*")))))))))
         (port-filter (lambda (serv-buf)
                        (lambda (process output)
                          (when (buffer-live-p serv-buf)
                            (with-current-buffer serv-buf
                              (insert output)
                              (when (string-match "Started nREPL server at 127.0.0.1:\\([0-9]+\\)" output)
                                (let ((port (string-to-number (match-string 1 output))))
                                  (setq nrepl-endpoint (list :host host :port port))
                                  (let ((client-proc (nrepl-start-client-process
                                                      host
                                                      port
                                                      process
                                                      (funcall repl-builder port))))
                                    (set-process-query-on-exit-flag client-proc nil)
                                    (when connected-callback
                                      (funcall connected-callback client-proc)))))))))))
    (with-current-buffer serv-buf
      (setq nrepl-is-server t
            nrepl-server-command cmd))
    (let ((serv-proc (start-file-process-shell-command "babashka-nrepl-server" serv-buf cmd)))
      (set-process-query-on-exit-flag serv-proc nil)
      (set-process-filter serv-proc (funcall port-filter serv-buf))
      (set-process-sentinel serv-proc 'nrepl-server-sentinel)
      (set-process-coding-system serv-proc 'utf-8-unix 'utf-8-unix)))
  nil)

(defun my-clojure-mode-hook ()
  (yas-minor-mode 1))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(provide 'custom-clj)
;;; custom-clj.el ends here
