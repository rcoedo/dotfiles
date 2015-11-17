;;; javascript-settings.el --- javascript layer

;;; Commentary:
;;; Configures javascript mode

;;; Code:
(require 'req-package)

(req-package js2-mode
  :defer t
  :require flycheck
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (setq-default js2-basic-offset 2)

  (add-hook 'js2-mode-hook 'ac-js2-mode))

(provide 'javascript-settings)
;;; javascript-settings.el ends here
