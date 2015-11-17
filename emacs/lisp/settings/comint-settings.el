;;; comint-settings.el --- Settings

;;; Commentary:
;;; Comint setting file.

;;; Code:
(require 'req-package)

(req-package comint
  :defer t
  :require evil
  :config
  (require 'evil)
  (require 'comint)
  (add-hook 'comint-mode-hook
            #'(lambda ()
                (evil-define-key 'insert comint-mode-map
                  (kbd "C-r") 'helm-comint-input-ring
                  (kbd "C-p") 'comint-previous-input
                  (kbd "C-n") 'comint-next-input))))

(provide 'comint-settings)
;;; comint-settings.el ends here
