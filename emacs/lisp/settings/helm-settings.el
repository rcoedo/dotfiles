;;; helm-settings.el --- Helm settings

;;; Commentary:
;;; Configures helm

;;; Code:
(require 'req-package)

(req-package helm-config)

(req-package helm
  :require helm-config
  :config
  (setq helm-ff-skip-boring-files t
        helm-boring-file-regexp-list   '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$" "\\.so$" "\\.a$"
                                         "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$" "bower_components" "node_modules")
        helm-boring-buffer-regexp-list '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*tramp" "\\*Minibuf" "\\*epc"))

  (advice-add 'helm-ff-filter-candidate-one-by-one
              :around (lambda (fcn file)
                        (unless (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
                          (funcall fcn file))))

  (define-key global-map (kbd "C-x C-f")    'helm-find-files)
  (define-key global-map (kbd "C-SPC")      'helm-mini)
  (define-key helm-map   (kbd "<tab>")      'helm-execute-persistent-action)
  (define-key helm-map   (kbd "C-i")        'helm-execute-persistent-action)
  (define-key helm-map   (kbd "C-z")        'helm-select-action)
  (define-key helm-map   (kbd "C-k")        'helm-previous-source)
  (define-key helm-map   (kbd "C-j")        'helm-next-source)

  (define-key helm-buffer-map     (kbd "<C-backspace>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (lambda (buffer) (kill-buffer buffer) (helm-mini))))))

  (define-key helm-buffer-map     (kbd "<C-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (call-other-window 'switch-to-buffer 'right)))))

  (define-key helm-buffer-map     (kbd "<C-S-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (call-other-window 'switch-to-buffer 'right)))))

  (define-key helm-find-files-map (kbd "<C-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (call-other-window 'find-file 'right)))))

  (define-key helm-find-files-map (kbd "<C-S-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (call-other-window 'find-file 'below)))))

  (helm-autoresize-mode t)
  (helm-mode t))

(req-package helm-projectile
  :require projectile helm grep
  :config
  (define-key helm-projectile-find-file-map (kbd "<C-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (call-other-window 'find-file 'right)))))

  (define-key helm-projectile-find-file-map (kbd "<C-S-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (call-other-window 'find-file 'below)))))

  (define-key projectile-command-map (kbd "p")   'helm-projectile-switch-project)
  (define-key projectile-command-map (kbd "s s") 'helm-projectile-ag))

(provide 'helm-settings)
;;; helm-settings.el ends here
