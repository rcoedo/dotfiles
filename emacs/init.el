;;; init.el --- entry point for emacs configuration

;;; Commentary:

;;; Code:
(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/interface/themes")

(load "platform/darwin.el")
(load "interface/main.el")
(load "settings/main.el")

(provide 'init)
;;; init.el ends here
