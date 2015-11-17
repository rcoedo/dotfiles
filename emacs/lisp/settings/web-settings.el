;;; web-settings.el --- web layer

;;; Commentary:
;;; Configures web mode

;;; Code:
(require 'req-package)

(req-package web-mode
  :defer t
  :require flycheck
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'"    . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'"       . web-mode))
  :config
  (setq-default flycheck-disabled-checkers
                 (append flycheck-disabled-checkers '(javascript-jshint)))

  (add-hook 'web-mode-hook
            #'(lambda ()
                (setq web-mode-markup-indent-offset 2)
                (setq web-mode-css-indent-offset 2)
                (setq web-mode-code-indent-offset 2)))

  (flycheck-add-mode 'javascript-eslint 'web-mode))

(provide 'web-settings)
;;; web-settings.el ends here
