;;; latex-settings.el --- Settings

;;; Commentary:
;;; Configures latex mode

;;; Code:
(require 'req-package)

(req-package latex-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.tex$\\'" . latex-mode))
  :config
  (add-hook 'latex-mode-hook
            #'(lambda ()
                (flyspell-mode t)
                (ispell-change-dictionary "english")
                (local-unset-key (kbd "C-SPC")))))

(provide 'latex-settings)
;;; latex-settings.el ends here
