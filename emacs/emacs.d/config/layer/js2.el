(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (setq-default js2-basic-offset 2))
