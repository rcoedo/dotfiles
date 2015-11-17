;;; main-settings.el --- Settings

;;; Commentary:
;;; Settings entry point.

;;; Code:
(load "functions/window.el")
(load "functions/buffer.el")
(load "functions/eshell.el")

;;; Setting layers
(load "settings/comint-settings.el")
(load "settings/company-settings.el")
(load "settings/eshell-settings.el")
(load "settings/flycheck-settings.el")
(load "settings/evil-settings.el")
(load "settings/elixir-settings.el")
(load "settings/smartparens-settings.el")
(load "settings/latex-settings.el")
(load "settings/elm-settings.el")
(load "settings/guide-key-settings.el")
(load "settings/javascript-settings.el")
(load "settings/rust-settings.el")
(load "settings/web-settings.el")
(load "settings/transpose-frame-settings.el")
(load "settings/magit-settings.el")
(load "settings/projectile-settings.el")
(load "settings/helm-settings.el")
;;;

;;; Global settings
(put 'dired-find-alternate-file 'disabled nil)
(windmove-default-keybindings)                    ; Move between windows with shift + arrow keys
(transient-mark-mode t)                           ; Show the mark as selected
(global-auto-revert-mode t)                       ; Reload buffers when they change outside emacs

(setq-default c-basic-offset 4
              truncate-lines nil
              prefer-coding-system 'utf-8
              indent-tabs-mode nil
              global-auto-revert-non-file-buffers t ;; Auto-revert
              auto-revert-verbose nil
              tab-width 4
              backup-inhibited t
              auto-save-default nil)
;;;

;;; Global bindings
(define-key key-translation-map (kbd "C-,") (kbd "C-x")) ;; These bring sanity to my fingers
(define-key key-translation-map (kbd "C-.") (kbd "C-c"))
(define-key key-translation-map "\e"        (kbd "C-g"))

(define-key global-map (kbd "M-n")           'next-non-emacs-buffer)
(define-key global-map (kbd "M-p")           'previous-non-emacs-buffer)
(define-key global-map (kbd "M-f")           'delete-other-windows)
(define-key global-map (kbd "M-e")           'eshell-projectile)
(define-key global-map (kbd "M-q")           'delete-window)
(define-key global-map (kbd "<C-backspace>") 'kill-buffer-and-window)
(define-key global-map (kbd "M-t")           'transpose-frame)
(define-key global-map (kbd "C-;")           'other-window)
(define-key global-map (kbd "C-x C-r")       'rename-current-buffer-file)
(define-key global-map (kbd "C-x C-k")       'delete-current-buffer-file)
;;;

(provide 'main-settings)
;;; main-settings.el ends here
