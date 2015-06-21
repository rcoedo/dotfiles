;; Global stuff
(setq inhibit-splash-screen t)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(menu-bar-mode -1)
(osx-clipboard-mode t)

(global-linum-mode t)
(setq linum-format 'dynamic)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d ")))
    ad-do-it))

(setq mac-command-modifier 'meta)

(setq backup-directory-alist `((".*" . "~/.saves"))
      auto-save-file-name-transforms '((".*" "~/.saves/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq system-uses-terminfo nil)
(prefer-coding-system 'utf-8)
(windmove-default-keybindings) 
(setq-default truncate-lines nil)
(setq-default indent-tabs-mode nil)
(setq search-highlight t)
(transient-mark-mode t)

(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer))

;; Evil stuff
(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(setq evil-leader/in-all-states t)
(evil-mode t)

(require 'evil-surround)
(global-evil-surround-mode t)

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

;; Helm stuff
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(define-key helm-find-files-map (kbd "C-c C-d") 'helm-ff-run-delete-file)
(define-key helm-find-files-map (kbd "C-c C-r") 'helm-ff-run-rename-file)
(define-key helm-find-files-map (kbd "C-c C-c") 'helm-ff-run-copy-file)

(helm-mode t)
(helm-autoresize-mode t)

(projectile-global-mode)
(setq projectile-enable-caching t)

(add-hook 'emacs-startup-hook #'(lambda ()
                                  (let ((default-directory (getenv "HOME")))
                                    (command-execute 'eshell)
                                    (bury-buffer))))

;; Guide key stuff
(require 'guide-key)
(setq guide-key/guide-key-sequence '("<SPC>")
      guide-key/recursive-key-sequence-flag t
      guide-key/idle-delay 0.1
      guide-key/popup-window-position 'bottom)
(guide-key-mode t)


;; Evil leader stuff
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>" 'avy-goto-word-1

  "ff"    'helm-find-files
  "fd"    'dired

  "pf"    'helm-projectile-find-file
  "pp"    'helm-projectile-switch-project
  "pa"    'helm-projectile-ag
  "pr"    'projectile-invalidate-cache

  "\\"    'evilnc-comment-or-uncomment-lines
  "cl"    'evilnc-quick-comment-or-uncomment-to-the-line
  "cc"    'evilnc-copy-and-comment-lines
  "cp"    'evilnc-comment-or-uncomment-paragraphs
  "cr"    'comment-or-uncomment-region
  "cv"    'evilnc-toggle-invert-comment-line-by-line
  "co"    'evilnc-comment-operator

  "jj"    'helm-buffers-list

  "sh"    'evil-search-highlight-persist-remove-all

  "i"     'package-install)
