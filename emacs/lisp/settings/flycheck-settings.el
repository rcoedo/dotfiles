;;; flycheck-settings.el --- flycheck layer

;;; Commentary:
;;; Configures flycheck

;;; Code:
(require 'req-package)

(req-package flycheck
  :defer t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(provide 'flycheck-settings)
;;; flycheck-settings.el ends here
