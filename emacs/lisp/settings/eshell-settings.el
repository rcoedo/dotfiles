;;; eshell-settings.el --- Settings

;;; Commentary:
;;; Configuration for eshell

;;; Code:
(require 'req-package)

(req-package eshell
  :defer t
  :require evil
  :config
  (setq eshell-history-size 1000
        eshell-prompt-function #'(lambda nil (concat (getenv "USER") "@" (system-name) ":"
                                                     (abbreviate-file-name (eshell/pwd))
                                                     (if (= (user-uid) 0) " # " " $ "))))

  (add-hook 'eshell-mode-hook #'(lambda ()
                                  (evil-define-key 'insert eshell-mode-map
                                    (kbd "C-a") 'eshell-maybe-bol
                                    (kbd "C-r") 'helm-eshell-history
                                    (kbd "C-p") 'eshell-previous-matching-input-from-input
                                    (kbd "C-n") 'eshell-next-matching-input-from-input)

                                  (company-mode -1)

                                  (defalias 'ff 'find-file)
                                  (defalias 'd  'dired))))

(provide 'eshell-settings)
;;; eshell-settings.el ends here
