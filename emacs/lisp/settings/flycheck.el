;;; flycheck.el --- flycheck layer

;;; Commentary:
;;; Configures flycheck

;;; Code:
(require 'req-package)

(req-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(provide 'flycheck)
;;; flycheck.el ends here
