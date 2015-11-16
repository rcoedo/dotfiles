;;; javascript.el --- javascript layer

;;; Commentary:
;;; Configures javascript mode

;;; Code:
(require 'req-package)

(req-package js2-mode
  :require flycheck
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'ac-js2-mode)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default js2-basic-offset 2))

(provide 'javascript)
;;; javascript.el ends here
