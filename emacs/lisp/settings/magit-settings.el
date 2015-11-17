;;; magit-settings.el --- Settings

;;; Commentary:
;;; Configuration for magit.

;;; Code:
(require 'req-package)

(req-package magit
  :defer t
  :config
  (global-set-key (kbd "C-c g s") 'magit-status)
  (global-set-key (kbd "C-c g l l") 'magit-log)
  (global-set-key (kbd "C-c g l c") 'magit-log-current))

(provide 'magit-settings)
;;; magit-settings.el ends here
