;;; company-settings.el --- Company layer

;;; Commentary:
;;; Configures company mode

;;; Code:
(require 'req-package)

(req-package company
  :defer t
  :require helm-company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'global-company-mode-hook
            #'(lambda ()
                (setq company-idle-delay 0
                      company-dabbrev-downcase nil)

                (define-key company-active-map (kbd "M-n") nil)
                (define-key company-active-map (kbd "M-p") nil)
                (define-key company-active-map (kbd "\C-n") 'company-select-next)
                (define-key company-active-map (kbd "\C-p") 'company-select-previous))))

(provide 'company-settings)
;;; company-settings.el ends here
