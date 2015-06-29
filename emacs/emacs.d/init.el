;;; init.el --- entry point for emacs configuration

;;; Commentary:

;;; Code:
(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/config/theme")
(add-to-list 'load-path "~/.emacs.d/config")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(load "core.el")

(load "layer/flycheck.el")
(load "layer/company.el")
(load "layer/elixir.el")
(load "layer/javascript.el")
(load "layer/rust.el")
(load "layer/web.el")
(load "layer/modeline.el")

(provide 'init)
;;; init.el ends here
