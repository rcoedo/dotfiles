(load "functions/window.el")

(setq backup-directory-alist `((".*" . "~/.saves"))
      auto-save-file-name-transforms '((".*" "~/.saves/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      electric-pair-mode t
      search-highlight t
      transient-mark-mode t
      prefer-coding-system 'utf-8)

(setq-default c-basic-offset 4
              truncate-lines nil
              indent-tabs-mode nil
              tab-width 4)

(put 'dired-find-alternate-file 'disabled nil)
(windmove-default-keybindings) 

;; Unbind list-buffers. We don't want that fucker
(dolist (key '("\C-x\C-b"))
  (global-unset-key key))

;; Bind some buffer stuff
(define-key global-map (kbd "M-n") 'next-non-emacs-buffer)
(define-key global-map (kbd "M-p") 'previous-non-emacs-buffer)

;; Always run eshell
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (let ((default-directory (getenv "HOME")))
                                    (command-execute 'eshell)
                                    (bury-buffer))))

;; load ~/.emacs.d/settings
(load "settings/evil.el")
(load "settings/helm.el")
(load "settings/flycheck.el")
(load "settings/transpose-frame.el")
(load "settings/ace-window.el")
(load "settings/company.el")
(load "settings/elixir.el")
(load "settings/javascript.el")
(load "settings/rust.el")
(load "settings/elm.el")
(load "settings/web.el")
(load "settings/magit.el")
(load "settings/guide-key.el")
