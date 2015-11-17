;;; init.el --- entry point for emacs configuration

;;; Commentary:

;;; Code:
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/interface/themes")
(require 'req-package)

(load "platform/darwin-platform.el")
(load "interface/main.el")
(load "settings/main-settings.el")

(req-package-finish)
(provide 'init)
;;; init.el ends here
(put 'erase-buffer 'disabled nil)
