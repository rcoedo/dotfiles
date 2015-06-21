(use-package company
             :ensure t
             :ensure helm-company
             :config
             (add-hook 'after-init-hook 'global-company-mode)
             (setq company-idle-delay 0)
             (define-key company-active-map (kbd "M-n") nil)
             (define-key company-active-map (kbd "M-p") nil)
             (define-key company-active-map (kbd "\C-n") 'company-select-next)
             (define-key company-active-map (kbd "\C-p") 'company-select-previous))
