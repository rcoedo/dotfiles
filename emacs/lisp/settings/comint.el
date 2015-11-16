;;; comint.el --- Settings

;;; Commentary:
;;; Comint setting file.

;;; Code:
(require 'comint)

(require 'evil)
(require 'evil-common)

(add-hook 'comint-mode-hook
          #'(lambda ()
              (evil-define-key 'insert comint-mode-map
                (kbd "C-r") 'helm-comint-input-ring
                (kbd "C-p") 'comint-previous-input
                (kbd "C-n") 'comint-next-input)))


(provide 'comint)
;;; comint.el ends here
