(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/config/theme")
(add-to-list 'load-path "~/.emacs.d/config")

(eval-when-compile
  (require 'use-package))

(load "core.el")
(load "layer/company.el")
(load "layer/elixir.el")
(load "layer/js2.el")
(load "layer/powerline.el")
(load "layer/color.el")
