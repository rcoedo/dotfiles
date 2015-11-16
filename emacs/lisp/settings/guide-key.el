;;; guide-key.el --- Settings

;;; Commentary:
;;; Configure guide-key.

;;; Code:
(require 'req-package)

(req-package guide-key
  :config
  (setq guide-key/guide-key-sequence '("<SPC>" "C-c")
        guide-key/recursive-key-sequence-flag t
        guide-key/idle-delay 0.1
        guide-key/popup-window-position 'bottom)
  (guide-key-mode t))

(provide 'guide-key.el)
;;; guide-key.el ends here
