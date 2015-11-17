;;; smartparens-settings.el --- smartparens settings

;;; Commentary:
;;; Configures smartparens

;;; Code:
(require 'req-package)

(req-package smartparens
  :config
  (smartparens-global-mode))

(provide 'smartparens-settings)
;;; smartparens-settings.el ends here
