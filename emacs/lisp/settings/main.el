(load "functions/window.el")

(setq backup-directory-alist `((".*" . "~/.saves"))
      auto-save-file-name-transforms '((".*" "~/.saves/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      search-highlight t
      prefer-coding-system 'utf-8)

(setq-default c-basic-offset 4
              truncate-lines nil
              indent-tabs-mode nil
              tab-width 4)

(put 'dired-find-alternate-file 'disabled nil)
(windmove-default-keybindings) 
(electric-pair-mode t)
(transient-mark-mode t)

;; load ~/.emacs.d/settings
(load "settings/evil.el")
(load "settings/eshell.el")
(load "settings/helm-projectile.el")
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
(define-key global-map (kbd "M-n") 'next-non-emacs-buffer)
(define-key global-map (kbd "M-p") 'previous-non-emacs-buffer)
(define-key global-map (kbd "C-;") 'other-window)

;; ;; Run eshell
;; (add-hook 'emacs-startup-hook #'(lambda ()
;;                                   (let ((default-directory (getenv "HOME")))
;;                                     (command-execute 'eshell)
;;                                     (bury-buffer))))
