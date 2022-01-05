;;; custom-centaur-tabs --- custom configuration for centaur tabs
;;; Commentary:
;;; Code:

(require 'centaur-tabs)

(centaur-tabs-mode t)
(centaur-tabs-headline-match)
;; (centaur-tabs-build-helm-source)

;; some buffers to ignore that are essentially never needed
(setq *centaur-tabs-ignore-buffers* '("*scratch*"
                                      "*GNU Emacs*"
                                      "*inferior-lisp*"
                                      "*slime-events*"))

(setq centaur-tabs-style "bar")
;; (setq centaur-tabs-show-count t)
;; (setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'left)
(setq centaur-tabs-cycle-scope 'tabs)
;; (setq awesome-tab-dark-unselected-blend 0.7)
;; (setq awesome-tab-dark-active-bar-color "#F62459")
;; (setq awesome-tab-active-bar-width 5)
;; (setq awesome-tab-show-tab-index t)

(global-set-key (kbd "s-1") 'centaur-tabs-select-visible-tab)
(global-set-key (kbd "s-2") 'centaur-tabs-select-visible-tab)
(global-set-key (kbd "s-3") 'centaur-tabs-select-visible-tab)
(global-set-key (kbd "s-4") 'centaur-tabs-select-visible-tab)
(global-set-key (kbd "s-5") 'centaur-tabs-select-visible-tab)
(global-set-key (kbd "s-6") 'centaur-tabs-select-visible-tab)
(global-set-key (kbd "s-7") 'centaur-tabs-select-visible-tab)
(global-set-key (kbd "s-8") 'centaur-tabs-select-visible-tab)
(global-set-key (kbd "s-9") 'centaur-tabs-select-visible-tab)
(global-set-key (kbd "s-0") 'centaur-tabs-select-visible-tab)

(defun centaur-tabs-hide-tab (x)
  (let ((name (format "%s" x)))
    (or (member name *centaur-tabs-ignore-buffers*)
        (string-prefix-p "*helm" name))))

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules.
Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
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
           ((and (>= (length name) 5)
                 (string-equal "magit" (substring name 0 5)))
            "magit")
           (t "user")))))

(global-set-key (kbd "M-s-1") (lambda () (interactive) (centaur-tabs-switch-group "user")))
(global-set-key (kbd "M-s-2") (lambda () (interactive) (centaur-tabs-switch-group "repl")))
(global-set-key (kbd "M-s-3") (lambda () (interactive) (centaur-tabs-switch-group "emacs")))
(global-set-key (kbd "M-s-4") (lambda () (interactive) (centaur-tabs-switch-group "dired")))
(global-set-key (kbd "M-s-5") (lambda () (interactive) (centaur-tabs-switch-group "magit")))

(provide 'custom-centaur-tabs)
;;; custom-centaur-tab.el ends here
