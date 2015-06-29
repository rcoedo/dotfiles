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

(define-key evil-normal-state-map (kbd "C-p") 'helm-find-files)
(define-key evil-normal-state-map (kbd "C-b") 'helm-mini)
(define-key evil-normal-state-map (kbd "C-SPC") 'switch-to-previous-buffer)
(define-key evil-normal-state-map (kbd "TAB") 'evil-window-next)

;; Helm stuff
(require 'helm)
(require 'helm-config)
(require 'helm-projectile)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)
(define-key helm-map (kbd "C-k") 'helm-previous-source)
(define-key helm-map (kbd "C-j") 'helm-next-source)

(define-key helm-find-files-map (kbd "C-c C-d") 'helm-ff-run-delete-file)
(define-key helm-find-files-map (kbd "C-c C-r") 'helm-ff-run-rename-file)
(define-key helm-find-files-map (kbd "C-c C-c") 'helm-ff-run-copy-file)

(helm-mode t)
(helm-autoresize-mode t)
(setq helm-ff-skip-boring-files t)
(setq helm-boring-file-regexp-list
  '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
    "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$" "bower_components"
    "node_modules"))
(setq helm-boring-buffer-regexp-list
  '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*tramp" "\\*Minibuf" "\\*epc"))

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


(require 'direx)
(add-hook 'direx:direx-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "O") 'direx:expand-item-recursively)
              (define-key evil-normal-state-local-map (kbd "r") 'direx:refresh-whole-tree)
              (define-key evil-normal-state-local-map (kbd "c") 'direx:jump-to-directory)
              (define-key evil-normal-state-local-map (kbd "RET") 'direx:toggle-item)))

(defun open-direx-tree ()
  (interactive)
  (split-window-horizontally (floor (* 0.10 (window-width))))
  (direx:jump-to-directory))

;; Evil leader stuff
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>" 'avy-goto-word-1

  "yy"    'helm-show-kill-ring

  "ff"    'helm-find-files
  "fd"    'dired
  "ft"    'open-direx-tree
  
  "pf"    'helm-projectile-find-file
  "pp"    'helm-projectile-switch-project
  "pa"    'helm-projectile-ag
  "pr"    'projectile-invalidate-cache
  "pb"    'helm-browse-project
  "pd"    'projectile-dired

  "bb"    'helm-buffers-list

  "\\"    'evilnc-comment-or-uncomment-lines
  "cl"    'evilnc-quick-comment-or-uncomment-to-the-line
  "cc"    'evilnc-copy-and-comment-lines
  "cp"    'evilnc-comment-or-uncomment-paragraphs
  "cr"    'comment-or-uncomment-region
  "cv"    'evilnc-toggle-invert-comment-line-by-line
  "co"    'evilnc-comment-operator

  "jj"    'helm-buffers-list

  "sh"    'evil-search-highlight-persist-remove-all

  "|"     'split-window-right
  "-"     'split-window-vertically
  "<DEL>" 'delete-window

  "TAB" 'transpose-frame

  "i"     'package-install)

(provide 'core)
;;; core.el ends here
