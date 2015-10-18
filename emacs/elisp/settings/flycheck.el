;;; flycheck.el --- flycheck layer

;;; Commentary:
;;; Configures flycheck

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package flycheck
  ;;; Load flycheck layer
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(provide 'flycheck)
;;; flycheck.el ends here
