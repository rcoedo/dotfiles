;; -*- lexical-binding: t -*-
(load "functions/window.el")

(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(require 'grep)

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

(setq projectile-globally-ignored-directories
      (append '(".cask")
       projectile-globally-ignored-files))

(setq projectile-project-root-files
      (append '("init.el")
       projectile-project-root-files))

;; Helper functions
(defun helm-run-other-window (fun position)
  (lambda ()
    (interactive)
    (with-helm-alive-p
      (helm-exit-and-execute-action (call-other-window fun position)))))

(defun helm-kill-buffer-now ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action (lambda (buffer)
                                    (kill-buffer buffer)
                                    (helm-mini)))))

;; Bindings
(define-key helm-map (kbd "<tab>")   'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i")     'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")     'helm-select-action)
(define-key helm-map (kbd "C-k")     'helm-previous-source)
(define-key helm-map (kbd "C-j")     'helm-next-source)

(define-key global-map (kbd "C-x C-f")    'helm-find-files)
(define-key global-map (kbd "<C-return>") 'helm-mini)

(define-key projectile-command-map (kbd "p")   'helm-projectile-switch-project)
(define-key projectile-command-map (kbd "s s") 'helm-projectile-ag)

(define-key helm-buffer-map (kbd "<C-return>")    (helm-run-other-window 'switch-to-buffer 'right))
(define-key helm-buffer-map (kbd "<C-S-return>")  (helm-run-other-window 'switch-to-buffer 'below))
(define-key helm-buffer-map (kbd "<C-backspace>") 'helm-kill-buffer-now)

(let ((helm-open-file-other-window-right (helm-run-other-window 'find-file 'right))
      (helm-open-file-other-window-below (helm-run-other-window 'find-file 'below)))
  (define-key helm-find-files-map           (kbd "<C-return>")   helm-open-file-other-window-right)
  (define-key helm-find-files-map           (kbd "<C-S-return>") helm-open-file-other-window-below)
  (define-key helm-projectile-find-file-map (kbd "<C-return>")   helm-open-file-other-window-right)
  (define-key helm-projectile-find-file-map (kbd "<C-S-return>") helm-open-file-other-window-below))

(advice-add 'helm-ff-filter-candidate-one-by-one
        :around (lambda (fcn file)
                  (unless (string-match "\\(?:/\\|\\`\\)\\.\\{1,2\\}\\'" file)
                    (funcall fcn file))))
