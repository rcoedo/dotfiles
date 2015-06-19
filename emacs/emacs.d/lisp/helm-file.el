(require 'helm)
(require 'helm-config)
(require 'helm-projectile)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(define-key helm-find-files-map (kbd "C-c C-d") 'helm-ff-run-delete-file)
(define-key helm-find-files-map (kbd "C-c C-r") 'helm-ff-run-rename-file)
(define-key helm-find-files-map (kbd "C-c C-c") 'helm-ff-run-copy-file)

(helm-mode 1)
(helm-autoresize-mode 1)

(add-hook 'emacs-startup-hook #'(lambda ()
                                  (let ((default-directory (getenv "HOME")))
                                    (command-execute 'eshell)
                                    (bury-buffer))))
