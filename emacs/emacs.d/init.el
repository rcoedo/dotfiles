(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)
(add-to-list 'load-path "~/.emacs.d/lisp")

(load "global-file.el")
(load "evil-file.el")
(load "helm-file.el")
(load "elixir-file.el")

(require 'powerline)
(require 'powerline-evil)
(require 'moe-theme)
(powerline-moe-theme)
(moe-theme-set-color 'w/b)
(moe-dark)

(load "modal-keys.el")
