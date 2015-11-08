;;; elixir.el --- elixir layer

;;; Commentary:
;;; Configures elixir and alchemist

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package elixir-mode
  ;;; Load elixir layer
  :ensure t
  :ensure alchemist
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(provide 'elixir)
;;; elixir.el ends here
