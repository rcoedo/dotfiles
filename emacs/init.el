;;; init.el -- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(setq ad-redefinition-action 'accept)
(require 'req-package)

(req-package linum
  :init
  (hl-line-mode t)
  (global-linum-mode t)
  :config
  (progn
    (setq linum-format 'dynamic)

    (set-face-attribute 'linum nil :height 100)
    (defadvice linum-update-window (around linum-dynamic activate)
      "Number into string with enough spaces."
      (let* ((w (length (number-to-string
                         (count-lines (point-min) (point-max)))))
             (linum-format (concat " %" (number-to-string w) "d ")))
        ad-do-it))))

(req-package smart-mode-line
  :config
  (progn
    (setq sml/theme nil)
    (sml/setup)))

(req-package gruvbox-theme
  :require smart-mode-line
  :config
  (progn
    (load-theme 'gruvbox t)
    (set-frame-font (font-spec :family "Operator Mono" :size 16))
    (set-face-italic 'font-lock-comment-face t)
    (setq-default line-spacing 3)))

(req-package dash
  :force t)

(req-package rcoedo
  :force t)

(req-package general
  :force t
  :config
  (progn
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
                        "M-q"     'rcoedo-projectile-eshell-popup
                        "M-e"     'eval-expression
                        "M-f"     'flip-frame
                        "M-F"     'flop-frame
                        "M-r"     'rotate-frame-clockwise
                        "M-R"     'rotate-frame-anticlockwise
                        "M-i"     'yas-insert-snippet
                        "M-s"     'helm-projectile-ag
                        "M-t"     'helm-projectile-find-file
                        "M-p"     'helm-ghq-list
                        "M-b"     'helm-mini
                        "C-x C-f" 'helm-find-files
                        "M-D"     'helm-dash-at-point
                        "M-d"     'helm-dash
                        "M-x"     'helm-M-x
                        "H-1"     'windmove-left
                        "H-2"     'windmove-down
                        "H-3"     'windmove-up
                        "H-4"     'windmove-right
                        "\C-x2"   'rcoedo-split-below-other-window
                        "\C-x3"   'rcoedo-split-right-other-window)

    (mmap "<right>" nil
          "<left>"  nil
          "<down>"  nil
          "<up>"    nil)

    (nmap "C-p"          nil
          "<tab>"       'other-window
          "<backspace>" 'evil-ex-nohighlight
          "C-j"         'helm-resume
          "/"           'swiper)

    (imap "C-a" 'beginning-of-line
          "C-e" 'end-of-line
          "C-f" 'forward-char
          "C-b" 'backward-char
          "C-d" 'delete-char
          "C-n" 'next-line
          "C-p" 'previous-line
          "C-w" 'evil-delete
          "C-k" 'kill-line)

    (rcoedo-leader-key "jr"      'jump-to-register
                       "jd"      'dired-jump
                       "yy"      'helm-show-kill-ring
                       "SPC"     'ace-jump-char-mode

                       "es"      'evil-ex-sort
                       "ee"      '(lambda () (interactive) (find-file "~/.emacs.d/init.el"))

                       "bK"      'kill-buffer-and-window
                       "bk"      'kill-this-buffer
                       "bd"      'rcoedo-delete-current-file
                       "br"      'rcoedo-rename-current-file

                       "fs"      'helm-projectile-ag
                       "ft"      'helm-projectile-find-file
                       "ff"      'helm-find-files
                       "fp"      'helm-ghq-list
                       "fb"      'helm-mini)))

(req-package evil
  :config
  (progn
    (evil-mode t)

    (evil-select-search-module 'evil-search-module 'evil-search)
    (add-hook 'write-file-hooks 'delete-trailing-whitespace)

    (setq evil-want-fine-undo t)

    (defvar evil-mode-list
      '((eshell-mode           insert)
        (comint-mode           insert)
        (alchemist-iex-mode    insert)
        (magit-mode            emacs)
        (magit-status          emacs)
        (magit-log-mode        emacs)
        (magit-commit-mode     emacs)
        (magit-diff-mode       emacs)
        (magit-popup-mode      emacs)
        (magit-merge-popup     emacs)
        (magit-revision-mode   emacs)
        (git-commit-mode       insert)
        (cider-stacktrace-mode insert)))

    (dolist (mode evil-mode-list)
      (evil-set-initial-state (nth 0 mode) (nth 1 mode)))))

(req-package evil-surround
  :require evil
  :config
  (progn
    (global-evil-surround-mode t)
    (vmap "s" 'evil-surround-region)
    (nmap "s" 'evil-surround-edit)))

(req-package evil-commentary
  :require evil
  :config
  (progn
    (evil-commentary-mode)))

(req-package evil-matchit
  :require evil
  :config
  (progn
    (global-evil-matchit-mode t)))

(req-package evil-lisp-state
  :require evil
  :init
  (progn
    (setq evil-lisp-state-global t
          evil-lisp-state-enter-lisp-state-on-command nil))
  :config
  (progn
    (evil-lisp-state-leader "L")

    (general-define-key :keymaps 'evil-lisp-state
                        "o" 'lisp-state-insert-sexp-after
                        "O" 'lisp-state-insert-sexp-before)))

(req-package evil-org
  :init
  (progn
    (add-hook 'org-mode-hook 'evil-org-mode)))

(req-package ace-jump-mode)

(req-package projectile
  :require ghq
  :config
  (progn
    (setq projectile-enable-caching t
          projectile-switch-project-action 'projectile-dired
          projectile-ignored-project-function #'(lambda (project-root) 'true)
          projectile-completion-system 'helm
          projectile-globally-ignored-directories (append '(".cask") projectile-globally-ignored-files)
          projectile-project-root-files ())

    (projectile-global-mode)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(req-package ivy
  :config
  (progn
    (setq ivy-use-virtual-buffers t
          ivy-height 25)))

(req-package swiper
  :config
  (progn
    (setq swiper-goto-start-of-match t)))

(req-package helm
  :config
  (progn
    (setq helm-exit-idle-delay 0)
    (helm-autoresize-mode t)
    (helm-mode t)

    (general-define-key :keymaps 'helm-map
                        "<tab>"  'helm-execute-persistent-action)

    (general-define-key :keymaps 'helm-buffer-map
                        "<C-backspace>" 'rcoedo-helm-kill-buffers
                        "<C-return>"    'rcoedo-helm-switch-buffer-right
                        "<C-S-return>"  'rcoedo-helm-switch-to-buffer-below)

    (general-define-key :keymaps 'helm-find-files-map
                        "<C-return>"   'rcoedo-helm-find-file-right
                        "<C-S-return>" 'rcoedo-helm-find-file-below)))

(req-package helm-projectile
  :require projectile helm grep
  :config
  (progn
    (helm-projectile-toggle 1)

    (setq projectile-switch-project-action 'projectile-dired)

    (general-define-key :keymaps 'projectile-command-map
                       "s s" 'helm-projectile-ag
                       "p" 'helm-ghq-list)

    (general-define-key :keymaps 'helm-projectile-find-file-map
                        "<C-return>" 'rcoedo-helm-find-file-right
                        "<C-S-return>" 'rcoedo-helm-find-file-below)))

(req-package helm-ag
  :require helm
  :config
  (progn
    (general-define-key :keymaps 'helm-ag-map
                        "<C-return>"   'rcoedo-helm-ag-find-file-right
                        "<C-S-return>" 'rcoedo-helm-ag-find-file-below)))

(req-package helm-dash
  :require helm
  :config
  (progn
    (setq helm-dash-browser-func 'eww
          helm-dash-docsets-path "~/.emacs.d/docsets"
          helm-dash-common-docsets (helm-dash-installed-docsets))))

(req-package helm-css-scss
  :config
  (progn
    (rcoedo-mode-key :keymaps 'css-mode-map
                      "f" 'helm-css-scss)))

(req-package yasnippet
  :config
  (progn
    (yas-global-mode t)

    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

    (general-define-key :keymaps 'yas-minor-mode-map
                        "<tab>"       nil
                        "TAB"         nil
                        "<C-return>" 'yas-expand)))

(req-package expand-region
  :config
  (progn
    (nmap "-" 'er/expand-region)))

(req-package ghq)

(req-package markdown-mode
  :mode "\\.md\\'")

(req-package comint
  :config
  (progn
    (general-define-key :states  'insert
                        :keymaps 'comint-modemap
                        "C-r"    'helm-comint-input-ring
                        "C-p"    'comint-previous-input
                        "C-n"    'comint-next-input)))

(req-package company
  :require helm-company
  :config
  (progn
    (setq company-idle-delay 0
          company-dabbrev-downcase nil)

    (general-define-key :keymaps 'company-active-map
                        "M-n"   nil
                        "M-p"   nil
                        "C-n"  'company-select-next
                        "C-p"  'company-select-previous)

    (global-company-mode)))

(req-package eshell
  :require evil company
  :config
  (progn
    (setq eshell-history-size 1000
          eshell-aliases-file "~/.emacs.d/eshell-aliases"
          eshell-prompt-function 'rcoedo-eshell-prompt)

    (general-define-key :states  'insert
                        :keymaps 'eshell-mode-map
                        "C-a"    'rcoedo-eshell-maybe-bol
                        "C-r"    'helm-eshell-history
                        "C-p"    'eshell-previous-matching-input-from-input
                        "C-n"    'eshell-next-matching-input-from-input)

    (company-mode -1)

    (defalias 'ff 'find-file)
    (defalias 'd  'dired)
    (defalias 'x  'kill-buffer-and-window)))

(req-package flycheck
  :require flycheck-cask
  :config
  (progn
    (global-flycheck-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-cask-setup)))

(req-package alchemist
  :require elixir-mode
  :init
  (progn
    (add-hook 'elixir-mode-hook 'alchemist-mode)))

;; (req-package 'elixir-format
;;   :require elixir-mode
;;   :init
;;   (progn
;;     (add-hook 'elixir-mode-hook
;;               (lambda () (add-hook 'before-save-hook elixir-format-before-save)))))

(req-package elixir-mode
  :require smartparens
  :config
  (progn
    (setq blink-matching-delay 0.1)

    (sp-with-modes '(elixir-mode)
      (sp-local-pair "->" "end"
                     :when '(("RET"))
                     :post-handlers '(:add rcoedo-elixir-do-end-close-action)
                     :actions '(insert))

      (sp-local-pair "do" "end"
                     :when '(("SPC" "RET"))
                     :post-handlers '(:add rcoedo-elixir-do-end-close-action)
                     :actions '(insert)))))

(req-package smartparens-config
  :config
  (progn
    (smartparens-global-mode)))

(req-package tex-mode
  :mode ("\\.tex$'" . latex-mode)
  :config
  (progn
    (setq ispell-dictionary "english")
    (add-hook 'latex-mode-hook 'flyspell-mode)))

(req-package elm-mode
  :mode "\\.elm$'"
  :require flycheck company
  :config
  (progn
    (setq elm-format-on-save t)
    (add-to-list 'company-backends 'company-elm)
    (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
    (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)))

(req-package lua-mode
  :mode "\\.lua$'"
  :interpreter "lua"
  :require flycheck)

(req-package which-key
  :config
  (setq which-key-idle-delay 0.5)

  (which-key-mode))

(req-package pyenv-mode
  :config
  (progn
    (add-hook 'python-mode-hook 'pyenv-mode)))

(req-package enh-ruby-mode
  :mode "\\.rb$'"
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
    (add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
    (add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
    (add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
    (add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
    (add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

    (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))))

(req-package anaconda-mode
  :require company
  :config
  (progn
    (add-to-list 'company-backends 'company-anaconda)
    (add-hook 'python-mode-hook 'anaconda-mode)))

(req-package
  :mode "\\.hs$'"
  :commands general haskell-mode
  :config
  (progn
    (setq haskell-process-suggest-remove-import-lines t
          haskell-process-auto-import-loaded-modules t
          haskell-process-log t
          haskell-process-type 'cabal-repl)

    (rcoedo-mode-key :keymaps '(haskell-mode-map haskell-cabal-mode-map)
                      "z" 'haskell-interactive-switch
                      "k" 'haskell-interactive-mode-clear
                      "pt" 'haskell-process-do-type
                      "pi" 'haskell-process-do-info
                      "pp" 'haskell-process-cabal-build
                      "pl" 'haskell-process-load-or-reload
                      "pc" 'haskell-process-cabal)

    (interactive-haskell-mode 1)
    (hindent-mode 1)))

(req-package prettier-js
  :init
  (progn
    (setq prettier-js-args '("--trailing-comma" "all" "--single-quote" "false" "--print-width" "120"))))

(req-package web-mode
  :require flycheck prettier-js
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)
         ("\\.ejs\\'"       . web-mode)
         ("\\.json?\\'"     . web-mode)
         ("\\.m?jsx?\\'"    . web-mode)
         ("\\.eex\\'"       . web-mode))
  :config
  (progn
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers '(javascript-jshint)))

    (setq web-mode-content-types-alist '(("jsx" . "\\.[m]?js[x]?\\'"))
          web-mode-auto-quote-style nil
          web-mode-enable-auto-pairing nil
          web-mode-enable-current-column-highlight t
          web-mode-enable-current-element-highlight t
          web-mode-attr-indent-offset 2
          web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2)

    (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
    (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))

    (sp-with-modes '(web-mode)
      (sp-local-pair "<" nil :actions :rem)
      (sp-local-pair "<%" " %>" :wrap "C-%"))

    (add-hook 'web-mode-hook #'(lambda ()
                                 (setq emmet-expand-jsx-className? t)
                                 (rainbow-delimiters-mode)
                                 (rcoedo-enable-minor-mode '("\\.m?jsx?\\'" . prettier-js-mode))
                                 (setq prettier-js-args '("--trailing-comma" "all" "--single-quote" "false" "--print-width" "120"))))))

(req-package tide
  :mode ("\\.ts$'" . typescript-mode)
  :require prettier-js
  :init
  (progn
    (add-hook 'typescript-mode-hook #'(lambda()
                                        (tide-setup)
                                        (setq typescript-indent-level 2)
                                        (tide-hl-identifier-mode t)
                                        (setq prettier-js-args '("--trailing-comma" "all" "--single-quote" "false" "--parser" "typescript" "--print-width" "120"))
                                        (prettier-js-mode t)))))

(req-package protobuf-mode
  :mode "\\.proto$'")

(req-package go-mode
  :mode "\\.go$'"
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)))

(req-package yaml-mode
  :mode "\\.yml$'")

(req-package css-mode
  :mode "\\.css$'"
  :config
  (progn
    (add-hook 'css-mode-hook 'prettier-js-mode)
    (setq css-indent-offset 2)))

(req-package scss-mode
  :mode "\\.scss$'"
  :config
  (progn
    (setq scss-compile-at-save nil)))

(req-package rainbow-mode
  :init
  (progn
    (rcoedo-add-hooks 'rainbow-mode '(css-mode-hook scss-mode-hook html-mode-hook web-mode-hook))))

(req-package emmet-mode
  :init
  (progn
    (rcoedo-add-hooks 'emmet-mode '(less-css-mode-hook scss-mode-hook web-mode-hook)))
  :config
  (progn
    (general-define-key :keymaps 'emmet-mode-keymap
                        "<C-return>" nil)))

(req-package magit
  :config
  (progn
    (general-define-key :keymaps 'magit-mode-map
                        "C-c g s"   'magit-status
                        "C-c g l l" 'magit-log
                        "C-c g l c" 'magit-log-current)))

(req-package popwin
  :config
  (progn
    (popwin-mode 1)))

(req-package org
  :config
  (progn
    (rcoedo-mode-key :keymaps 'org-mode-map
                      "t" 'org-babel-tangle)

    (general-define-key :keymaps org-mode-map
                        "<S-right>" nil
                        "<S-left>"  nil
                        "C-'"       nil)))

(req-package transpose-frame)

(req-package rainbow-delimiters)

(req-package lisp-mode
  :require rainbow-delimiters
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
  :config
  (progn
    (general-define-key :keymaps emacs-lisp-mode-map
                        "M-v" 'eval-defun)))

(req-package octave
  :mode ("\\.m$" . octave-mode)
  :config
  (progn
    (general-define-key :keymaps octave-mode-map
                        "M-v" 'octave-send-defun)))

(req-package eww
  :config
  (progn
    (general-define-key :states  'normal
                        :keymaps 'eww-mode-map
                        "q" 'quit-window)))

(req-package dired
  :config
  (progn
    (general-define-key :keymaps 'dired-mode-map
                        "M-s" nil)
    (setq dired-use-ls-dired nil)))

(req-package exec-path-from-shell
  :config
  (progn
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("GHQ_ROOT" "GOPATH"))))

(put 'dired-find-alternate-file 'disabled nil) ;; Allow the use of dired-find-alternate-file
(put 'erase-buffer 'disabled nil)              ;; Allow the use of erase-buffer
(transient-mark-mode t)                        ;; Show the mark as selected
(global-auto-revert-mode t)                    ;; Reload buffers when they change outside emacs
(menu-bar-mode -1)                             ;; Hide menu bar
(scroll-bar-mode -1)                           ;; Hide scroll bar
(tool-bar-mode -1)                             ;; Hide tool bar

(setq-default c-basic-offset 4
              truncate-lines nil
              prefer-coding-system 'utf-8
              indent-tabs-mode nil
              global-auto-revert-non-file-buffers t ;; Auto-revert
              auto-revert-verbose nil
              tab-width 4
              backup-inhibited t
              auto-save-default nil
              inhibit-splash-screen t
              menu-bar-mode -1)

(when (memq window-system '(mac ns)) ;; MacOS specific configuration
  (setq mac-command-modifier 'meta
        ns-use-native-fullscreen nil
        mac-option-modifier 'none
        system-uses-terminfo nil
        ring-bell-function 'ignore
        osx-clipboard-mode t)

  (general-define-key :keymaps 'key-translation-map
                      "˙"   (kbd "H-1")
                      "∆"   (kbd "H-2")
                      "˚"   (kbd "H-3")
                      "¬"   (kbd "H-4")))

(req-package-finish)

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (realgud yaml-mode go-mode yasnippet which-key web-mode transpose-frame tide smart-mode-line scss-mode req-package rainbow-mode rainbow-delimiters pyenv-mode protobuf-mode prettier-js popwin osx-clipboard markdown-mode magit lua-mode hindent helm-projectile helm-dash helm-css-scss helm-company helm-ag haskell-mode gruvbox-theme ghq general flycheck-elm flycheck-cask fish-mode expand-region exec-path-from-shell evil-surround evil-org evil-matchit evil-lisp-state evil-commentary enh-ruby-mode emmet-mode elm-mode el-get counsel company-anaconda alchemist ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
