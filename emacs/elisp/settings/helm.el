(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(require 'grep)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "C-k") 'helm-previous-source)
(define-key helm-map (kbd "C-j") 'helm-next-source)

(define-key global-map (kbd "C-x b") 'helm-mini)

(define-key projectile-command-map (kbd "p") 'helm-projectile-switch-project)
(define-key projectile-command-map (kbd "s s") 'helm-projectile-ag)

(setq helm-ff-skip-boring-files t
      projectile-enable-caching t
      projectile-switch-project-action 'projectile-dired
      projectile-require-project-root nil
      helm-boring-file-regexp-list
      '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
        "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$" "bower_components"
        "node_modules")
      helm-boring-buffer-regexp-list
      '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*tramp" "\\*Minibuf" "\\*epc"))

(helm-autoresize-mode t)
(helm-mode t)
(projectile-global-mode)
