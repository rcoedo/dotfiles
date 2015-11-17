;;; latex-settings.el --- Settings

;;; Commentary:
;;; Configures latex mode

;;; Code:
(require 'req-package)

(req-package latex-mode
  :defer t
  :config
  (setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))

  (add-hook 'latex-mode-hook
            #'(lambda ()
                (flyspell-mode t)
                (ispell-change-dictionary "english")
                (local-set-key (kbd "C-SPC") 'helm-mini))))

(provide 'latex-settings)
;;; latex-settings.el ends here
