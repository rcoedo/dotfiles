;;; elixir-settings.el --- elixir layer

;;; Commentary:
;;; Configures elixir and alchemist

;;; Code:
(require 'req-package)

(req-package elixir-mode
  :defer t
  :require smartparens
  :config
  (setq blink-matching-delay 0.1)

  (add-hook 'elixir-mode-hook 'alchemist-mode)

  (defun my-elixir-do-end-close-action (id action context)
    (when (eq action 'insert)
      (newline-and-indent)
      (forward-line -1)
      (indent-according-to-mode)))

  (sp-with-modes '(elixir-mode)
    (sp-local-pair "->" "end"
                   :when '(("RET"))
                   :post-handlers '(:add my-elixir-do-end-close-action)
                   :actions '(insert)))

  (sp-with-modes '(elixir-mode)
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(:add my-elixir-do-end-close-action)
                   :actions '(insert))))

(provide 'elixir-settings)
;;; elixir-settings.el ends here
