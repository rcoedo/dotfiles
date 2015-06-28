(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/config/theme")
(add-to-list 'load-path "~/.emacs.d/config")

(eval-when-compile
  (require 'use-package))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(load "core.el")
(load "layer/company.el")
(load "layer/elixir.el")
(load "layer/js2.el")
(load "layer/rust.el")
(load "layer/web.el")
(load "layer/modeline.el")
