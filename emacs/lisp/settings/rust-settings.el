;;; rust-settings.el --- rust layer

;;; Commentary:
;;; Configures rust mode

;;; Code:
(require 'req-package)

(req-package rust-mode
  :defer t
  :require flycheck-rust
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  :config
  (setq-default rust-basic-offset 4)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(provide 'rust-settings)
;;; rust-settings.el ends here
