;;; init.el -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Straight.el config
(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;;

;; Misc
(put 'dired-find-alternate-file 'disabled nil)          ;; Allow the use of dired-find-alternate-file
(put 'erase-buffer 'disabled nil)                       ;; Allow the use of erase-buffer
(transient-mark-mode)                                   ;; Show the mark as selected
(global-auto-revert-mode)                               ;; Reload buffers when they change outside emacs
(menu-bar-mode -1)                                      ;; Hide menu bar
(scroll-bar-mode -1)                                    ;; Hide scroll bar
(tool-bar-mode -1)                                      ;; Hide tool bar

(setq-default c-basic-offset 4
              truncate-lines nil
              prefer-coding-system 'utf-8
              indent-tabs-mode nil
              global-auto-revert-non-file-buffers t     ;; Auto-revert
              auto-revert-verbose nil
              tab-width 4
              backup-inhibited t
              auto-save-default nil
              inhibit-splash-screen t
              menu-bar-mode -1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)   ;; Line numbers in major modes derived from prog-mode
(add-hook 'write-file-hooks 'delete-trailing-whitespace) ;; Delete trailing spaces

 ;; MacOS specific configuration
(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'meta
        ns-use-native-fullscreen nil
        mac-option-modifier 'none
        system-uses-terminfo nil
        ring-bell-function 'ignore))
;;

;; Load helpers
(load "~/.emacs.d/helpers.el")

;; Theme
(use-package gruvbox-theme
  :init
  (set-frame-font (font-spec :family "Operator Mono" :size 16))
  (set-face-italic 'font-lock-comment-face t)
  (setq-default line-spacing 3)
  :config
  (load-theme 'gruvbox t))
;;

;; General
(use-package general
  :demand
  :config
  (general-evil-setup t t)

  (general-create-definer rcoedo-leader-key
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer rcoedo-mode-key
    :states '(normal insert visual emacs)
    :prefix "<SPC>m"
    :non-normal-prefix "<C-SPC>m")

  (general-define-key :keymaps 'key-translation-map
                      "C-," (kbd "C-x")
                      "C-." (kbd "C-c")
                      "\e"  (kbd "C-g"))

  (general-define-key "M-]"     'rcoedo-next-non-emacs-buffer
                      "M-["     'rcoedo-previous-non-emacs-buffer
                      "M-e"     'eval-expression
                      "\C-x2"   'rcoedo-split-below-other-window
                      "\C-x3"   'rcoedo-split-right-other-window)

  (mmap
    "<right>" nil
    "<left>"  nil
    "<down>"  nil
    "<up>"    nil)

  (nmap
    "C-p"          nil
    "<tab>"       'other-window)

  (imap
    "C-a" 'beginning-of-line
    "C-e" 'end-of-line
    "C-f" 'forward-char
    "C-b" 'backward-char
    "C-d" 'delete-char
    "C-n" 'next-line
    "C-p" 'previous-line
    "C-k" 'kill-line)

  (rcoedo-leader-key
    "jr"      'jump-to-register
    "ee"      '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))
    "bK"      'kill-buffer-and-window
    "bk"      'kill-this-buffer
    "bd"      'rcoedo-delete-current-file
    "br"      'rcoedo-rename-current-file))

(use-package yasnippet
  :general
  ("M-i"     'yas-insert-snippet)
  (:keymaps 'yas-minor-mode-map
            "<tab>"       nil
            "TAB"         nil
            "<C-return>" 'yas-expand)
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode t))

(use-package expand-region
  :general
  (nmap "-" 'er/expand-region))

(use-package smart-mode-line
  :init
  (setq sml/theme nil)
  :config
  (sml/setup))

(use-package helm
  :demand
  :general
  ("M-b"     'helm-mini)
  ("C-x C-f" 'helm-find-files)
  ("M-x"     'helm-M-x)
  (nmap "C-j" 'helm-resume)
  (rcoedo-leader-key
    "yy"      'helm-show-kill-ring
    "ff"      'helm-find-files
    "fb"      'helm-mini)
  (:keymaps 'helm-map
            "<tab>"  'helm-execute-persistent-action)
  (:keymaps 'helm-buffer-map
            "<C-backspace>" 'rcoedo-helm-kill-buffers
            "<C-return>"    'rcoedo-helm-switch-buffer-right
            "<C-S-return>"  'rcoedo-helm-switch-to-buffer-below)
  (:keymaps 'helm-find-files-map
            "<C-return>"   'rcoedo-helm-find-file-right
            "<C-S-return>" 'rcoedo-helm-find-file-below)
  :init
  (setq helm-exit-idle-delay 0)
  :config
  (helm-mode)
  (helm-autoresize-mode)
  (add-to-list 'helm-find-files-actions '("rc.Find file right" . rcoedo-find-file-right) :append)
  (add-to-list 'helm-find-files-actions '("rc.Find file below" . rcoedo-find-file-below) :append)

  (add-to-list 'helm-type-buffer-actions '("rc.Find buffer right" . rcoedo-switch-to-buffer-right) :append)
  (add-to-list 'helm-type-buffer-actions '("rc.Find buffer below" . rcoedo-switch-to-buffer-below) :append))

(use-package helm-projectile
  :after (projectile helm)
  :general
  ("M-s"      'helm-projectile-ag)
  ("M-t"      'helm-projectile-find-file)
  (rcoedo-leader-key
    "fs"      'helm-projectile-ag
    "ft"      'helm-projectile-find-file)
  (:keymaps 'projectile-command-map
            "s s" 'helm-projectile-ag)
  (:keymaps 'helm-projectile-find-file-map
            "<C-return>" 'rcoedo-helm-find-file-right
            "<C-S-return>" 'rcoedo-helm-find-file-below)
  :config
  (helm-projectile-toggle 1)
  (add-to-list 'helm-projectile-file-actions '("rc.Find file right" . rcoedo-find-file-right) t)
  (add-to-list 'helm-projectile-file-actions '("rc.Find file below" . rcoedo-find-file-below) t))

(use-package helm-ag
  :after helm
  :general
  (:keymaps 'helm-ag-map
            "<C-return>"   'rcoedo-helm-ag-find-file-right
            "<C-S-return>" 'rcoedo-helm-ag-find-file-below)
  :config
  (add-to-list 'helm-ag--actions '("rc.Ag Find file right" . rcoedo-helm-ag-find-file-right) t)
  (add-to-list 'helm-ag--actions '("rc.Ag Find file below" . rcoedo-helm-ag-find-file-below) t))

(use-package ghq
  :general
  ("M-p"     'helm-ghq-list)
  (rcoedo-leader-key
    "fp"     'helm-ghq-list)
  (:keymaps 'projectile-command-map
            "p" 'helm-ghq-list))

(use-package company
  :after general
  :general
  (:keymaps 'company-active-map
            "M-n"   nil
            "M-p"   nil
            "C-n"  'company-select-next
            "C-p"  'company-select-previous)
  :init
  (setq company-idle-delay 0
        company-dabbrev-downcase nil)
  :config
  (global-company-mode))

(use-package projectile
  :after general
  :general
  (:keymaps 'projectile-mode-map
            "C-c p" 'projectile-command-map)
  :init
  (setq projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired
        projectile-ignored-project-function #'(lambda (project-root) 'true)
        projectile-completion-system 'helm
        projectile-project-root-files ())
  :config
  (projectile-global-mode))

(use-package swiper
  :general
  (nmap "/" 'swiper)
  :init
  (setq swiper-goto-start-of-match t))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t
        ivy-height 25))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package evil
  :after undo-tree
  :demand
  :general
  (nmap
    "<backspace>" 'evil-ex-nohighlight)
  (imap
    "C-w" 'evil-delete)
  (rcoedo-leader-key
    "es"      'evil-ex-sort)
  :init
  (setq evil-want-fine-undo t
        evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (evil-set-undo-system 'undo-tree)
  (evil-select-search-module 'evil-search-module 'evil-search))

(use-package evil-surround
  :after evil
  :general
  (nmap "s" 'evil-surround-edit)
  (vmap "s" 'evil-surround-region)
  :config
  (global-evil-surround-mode))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode))

(use-package evil-lisp-state
  :after evil
  :hook emacs-lisp-mode
  :general
  (:keymaps 'evil-lisp-state
            "o" 'lisp-state-insert-sexp-after
            "O" 'lisp-state-insert-sexp-before)
  :init
  (setq evil-lisp-state-global t
        evil-lisp-state-enter-lisp-state-on-command nil)
  :config
  (evil-lisp-state-leader "L"))

(use-package evil-org
  :after evil
  :hook (org-mode . evil-org-mode))

(use-package smartparens
  :config
  (smartparens-global-mode))

(use-package smartparens-config
  :straight nil
  :after smartparens)

(use-package rainbow-mode
  :hook (css-mode html-mode web-mode))

(use-package transpose-frame
  :general
  ("M-f"     'flip-frame)
  ("M-F"     'flop-frame)
  ("M-r"     'rotate-frame-clockwise)
  ("M-R"     'rotate-frame-anticlockwise))

(use-package rainbow-delimiters
  :hook ((go-mode emacs-lisp-mode rjsx-mode) . rainbow-delimiters-mode))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package popwin
  :config
  (popwin-mode))

(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package eww
  :general
  (:keymaps 'eww-mode-map :states  'normal
            "q" 'quit-window))

(use-package dired
  :straight nil
  :general
  (rcoedo-leader-key
    "jd"      'dired-jump)
  (:keymaps 'dired-mode-map
            "M-s" nil)
  :init
  (setq dired-use-ls-dired nil))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package markdown-mode
  :mode "\\.md\\'"
  :general
  (nmap "<C-return>" 'markdown-table-align))

(use-package tex-mode
  :mode (("\\.tex$'" . latex-mode)))

(use-package flyspell-mode
  :straight nil
  :hook latex-mode
  :init
  (setq ispell-dictionary "english"))

(use-package svelte-mode
  :mode "\\.svelte$'")

(use-package lua-mode
  :mode "\\.lua$'"
  :interpreter "lua")

(use-package anaconda-mode
  :hook python-mode
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package blacken
  :after anaconda-mode
  :hook (python-mode . blacken-mode))

(use-package prettier-js
  :hook ((rjsx-mode css-mode) . prettier-js-mode)
  :init
  (setq prettier-js-args '("--trailing-comma" "all" "--single-quote" "false" "--print-width" "120")))

(use-package rjsx-mode
  :mode ("\\.m?jsx?\\'" . rjsx-mode)
  :init
  (setq js2-include-node-externs t
        js-indent-level 2))

(use-package protobuf-mode
  :mode "\\.proto$'")

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :mode "\\.go$'")

(use-package solidity-mode
  :mode "\\.sol$'")

(use-package yaml-mode
  :mode "\\.yml$'")

(use-package css-mode
  :mode "\\.css$'"
  :init
  (setq css-indent-offset 2))

(provide 'init)
;;; init.el ends here
