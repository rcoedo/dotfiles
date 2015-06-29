;;; rust.el --- rust layer

;;; Commentary:
;;; Configures rust mode

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package rust-mode
  :ensure t
  :ensure flycheck-rust
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setq-default rust-basic-offset 4)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(provide 'rust)
;;; rust.el ends here
