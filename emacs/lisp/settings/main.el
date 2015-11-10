(load "functions/window.el")
(load "functions/buffer.el")

(setq backup-directory-alist `((".*" . "~/.saves"))
      auto-save-file-name-transforms '((".*" "~/.saves/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      search-highlight t
      prefer-coding-system 'utf-8
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(setq-default c-basic-offset 4
              truncate-lines nil
              indent-tabs-mode nil
              tab-width 4)

(put 'dired-find-alternate-file 'disabled nil)
(windmove-default-keybindings) 
(electric-pair-mode t)
(transient-mark-mode t)
(global-auto-revert-mode t)

;; load ~/.emacs.d/settings
(load "settings/evil.el")
(load "settings/eshell.el")
(load "settings/comint.el")
(load "settings/helm-projectile.el")
(load "settings/popwin.el")
(load "settings/direx.el")
(load "settings/flycheck.el")
(load "settings/transpose-frame.el")
(load "settings/company.el")
(load "settings/elixir.el")
(load "settings/javascript.el")
(load "settings/rust.el")
(load "settings/elm.el")
(load "settings/web.el")
(load "settings/magit.el")
(load "settings/guide-key.el")
(load "settings/backup.el")

;; Bind some buffer stuff
(define-key global-map (kbd "M-n")        'next-non-emacs-buffer)
(define-key global-map (kbd "M-p")        'previous-non-emacs-buffer)
(define-key global-map (kbd "M-f")        'delete-other-windows)
(define-key global-map (kbd "M-q")        'delete-window)
(define-key global-map (kbd "<C-backspace>") 'kill-buffer-and-window)
(define-key global-map (kbd "M-t")        'transpose-frame)
(define-key global-map (kbd "C-;")        'other-window)
(define-key global-map (kbd "C-x C-r")    'rename-current-buffer-file)
(define-key global-map (kbd "C-x C-k")    'delete-current-buffer-file)
