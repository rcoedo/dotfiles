;;; projectile-settings.el --- Projectile settings

;;; Commentary:
;;; Configures projectile

;;; Code:
(require 'req-package)

(req-package projectile
  :config
  (setq projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired
        projectile-require-project-root nil
        projectile-globally-ignored-directories (append '(".cask") projectile-globally-ignored-files)
        projectile-project-root-files (append '("init.el") projectile-project-root-files))
  (projectile-global-mode))

(provide 'projectile-settings)
;;; projectile-settings.el ends here
