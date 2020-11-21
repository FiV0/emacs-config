;;; custom-py --- custom configuration for python
;;; Commentary:
;;; Code:

(defun company-jedi-setup ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'company-jedi-setup)

(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook 'jedi:setup)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i")

(setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)

(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(provide 'custom-py)
;;; custom-py.el ends here
