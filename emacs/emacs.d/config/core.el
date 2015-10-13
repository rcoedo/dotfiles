;;; core.el --- Basic emacs configuration

;;; Commentary:

;;; Code:

;; Global stuff
(setq inhibit-splash-screen t)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(menu-bar-mode -1)
(osx-clipboard-mode t)
(setq-default c-basic-offset 4
              tab-width 4)

(electric-pair-mode 1)

(require 'transpose-frame)

(put 'dired-find-alternate-file 'disabled nil)

(global-linum-mode t)
(setq linum-format 'dynamic)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
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

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(dolist (key '("\C-x\C-b"))
  (global-unset-key key))

(define-key global-map (kbd "M-n") 'previous-multiframe-window)
(define-key global-map (kbd "M-p") 'switch-to-previous-buffer)
(define-key global-map (kbd "C-x b") 'helm-mini)

;; Helm stuff
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(require 'grep)
(setq projectile-require-project-root nil)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "C-k") 'helm-previous-source)
(define-key helm-map (kbd "C-j") 'helm-next-source)

(define-key projectile-command-map (kbd "p") 'helm-projectile-switch-project)
(define-key projectile-command-map (kbd "s s") 'helm-projectile-ag)

(helm-autoresize-mode t)
(setq helm-ff-skip-boring-files t)
(setq helm-boring-file-regexp-list
  '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
    "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$" "bower_components"
    "node_modules"))
(setq helm-boring-buffer-regexp-list
  '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*tramp" "\\*Minibuf" "\\*epc"))

(helm-mode t)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)

(add-hook 'emacs-startup-hook #'(lambda ()
                                  (let ((default-directory (getenv "HOME")))
                                    (command-execute 'eshell)
                                    (bury-buffer))))

;; Guide key stuff
(require 'guide-key)
(setq guide-key/guide-key-sequence '("<SPC>" "C-c")
      guide-key/recursive-key-sequence-flag t
      guide-key/idle-delay 0.1
      guide-key/popup-window-position 'bottom)
(guide-key-mode t)

(defun open-direx-tree ()
  (interactive)
  (split-window-horizontally (floor (* 0.10 (window-width))))
  (direx:jump-to-directory))

;; ;; Evil leader stuff
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>" 'avy-goto-word-1

  "yy"    'helm-show-kill-ring

  "cc"    'evilnc-comment-or-uncomment-lines
  "cp"    'evilnc-copy-and-comment-lines
  "cb"    'evilnc-comment-or-uncomment-paragraphs
  "co"    'evilnc-comment-operator

  "sh"    'evil-search-highlight-persist-remove-all

  "TAB"   'transpose-frame

  "i"     'package-install)

(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g l l") 'magit-log)
(global-set-key (kbd "C-c g l c") 'magit-log-current)

(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'magit-status-mode 'emacs)
(evil-set-initial-state 'magit-log-mode 'emacs)
(evil-set-initial-state 'magit-commit-mode 'emacs)
(evil-set-initial-state 'magit-diff-mode 'emacs)
(evil-set-initial-state 'magit-popup-mode 'emacs)
(evil-set-initial-state 'magit-merge-popup 'emacs)
(evil-set-initial-state 'magit-revision-mode 'emacs)
(evil-set-initial-state 'git-commit-mode 'insert)
(provide 'core)
;;; core.el ends here
