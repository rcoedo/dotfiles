;;; javascript.el --- javascript layer

;;; Commentary:
;;; Configures javascript mode

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package js2-mode
  :ensure t
  :ensure jsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'ac-js2-mode)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))

  ;; use eslint for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default js2-basic-offset 2))

(provide 'javascript)
;;; javascript.el ends here
