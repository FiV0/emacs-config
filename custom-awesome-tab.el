;;; custom-awesome-tab --- custom configuration for awesome tab
;;; Commentary:
;;; Code:

(require 'awesome-tab)

(awesome-tab-mode t)
(awesome-tab-build-helm-source)

;; some buffers to ignore that are essentially never needed
(setq *awesome-tab-ignore-buffers* '("*scratch*"
                                     "*GNU Emacs*"
                                     "*inferior-lisp*"
                                     "*slime-events*"))
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
     (cond ((or (and (>= (length name) 6) (string-equal "*slime" (substring name 0 6)))
                (and (>= (length name) 6) (string-equal "*cider" (substring name 0 6)))
                (and (>= (length name) 6) (string-equal "*ielm" (substring name 0 5)))
                (and (>= (length name) 7) (string-equal "*Python" (substring name 0 7))))
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

(provide 'custom-awesome-tab)
;;; custom-awesome-tab.el ends here
