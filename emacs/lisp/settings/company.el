;;; company.el --- company layer

;;; Commentary:
;;; Configures company mode

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package company
  :ensure t
  :ensure helm-company
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  (defvar company-dabbrev-downcase)
  (setq company-idle-delay 0
        company-dabbrev-downcase nil)

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous))

(provide 'company)
;;; company.el ends here
