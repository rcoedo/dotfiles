;;; -*- lexical-binding: t -*-


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(setq ad-redefinition-action 'accept)
(require 'req-package)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(when (memq window-system '(mac ns))
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GHQ_ROOT")
  (exec-path-from-shell-copy-env "GOPATH")

  (setq mac-command-modifier 'meta
        mac-option-key-is-meta nil
        ns-use-native-fullscreen nil
        mac-option-modifier 'none
        system-uses-terminfo nil
        ring-bell-function 'ignore
        osx-clipboard-mode t)

  (define-key key-translation-map (kbd "˙") (kbd "H-1"))
  (define-key key-translation-map (kbd "∆") (kbd "H-2"))
  (define-key key-translation-map (kbd "˚") (kbd "H-3"))
  (define-key key-translation-map (kbd "¬") (kbd "H-4")))

(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

(setq inhibit-splash-screen t
      menu-bar-mode -1)

(req-package linum
  :init
  (hl-line-mode t)
  (global-linum-mode t)
  :config
  (setq linum-format 'dynamic)

  (set-face-attribute 'linum nil :height 100)
  (defadvice linum-update-window (around linum-dynamic activate)
    "Number into string with enough spaces."
    (let* ((w (length (number-to-string
                       (count-lines (point-min) (point-max)))))
           (linum-format (concat " %" (number-to-string w) "d ")))
      ad-do-it)))

(req-package smart-mode-line
  :config
  (progn
    (setq sml/theme nil)
    (sml/setup)))

(req-package gruvbox-theme
  :require smart-mode-line
  :config
  (load-theme 'gruvbox t)
  (set-frame-font (font-spec :family "Operator Mono" :size 16))
  (set-face-italic 'font-lock-comment-face t)
  (setq-default line-spacing 3))

(defun rcoedo/enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
      (funcall (cdr my-pair)))))

(defun rcoedo/window/call-other (fun position)
  (lambda (args)
    (select-window (if (eq position 'below) (split-window-below) (split-window-right)))
    (funcall fun args)))

(defun rcoedo/buffer/rename-current-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun rcoedo/buffer/delete-current-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rcoedo/buffer/emacs-buffer-p (name)
  "Returns true if the name matches an Emacs buffer."
  (string-match-p "\\*.*\\*" name))

(defun rcoedo/buffer/next-non-emacs-buffer (&optional original)
  "Similar to next-buffer, but ignores emacs buffer such as *scratch*, *messages* etc."
  (interactive)
  (let ((tmp-orig (or original (buffer-name))))
    (next-buffer)
    (if (and
         (not (eq (buffer-name) tmp-orig))
         (rcoedo/buffer/emacs-buffer-p (buffer-name)))
        (rcoedo/buffer/next-non-emacs-buffer tmp-orig))))

(defun rcoedo/buffer/previous-non-emacs-buffer (&optional original)
  "Similar to previous-buffer, but ignores emacs buffer such as *scratch*, *messages* etc."
  (interactive)
  (let ((tmp-orig (or original (buffer-name))))
    (previous-buffer)
    (if (and
         (not (eq (buffer-name) tmp-orig))
         (rcoedo/buffer/emacs-buffer-p (buffer-name)))
        (rcoedo/buffer/previous-non-emacs-buffer tmp-orig))))

(defun rcoedo/eshell/maybe-bol ()
      (interactive)
      (let ((p (point)))
        (eshell-bol)
        (if (= p (point))
            (beginning-of-line))))

(defun rcoedo/eshell/eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun rcoedo/eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(defun rcoedo/eshell/projectile-eshell-popup ()
  (interactive)
  (let* ((height (/ (window-total-height) 3))
         (name   (projectile-project-root)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (cd (projectile-project-root)))
  (rcoedo/eshell/clear)
  (insert "ls")
  (eshell-send-input))

(defun rcoedo/eshell/projectile-eshell ()
  (interactive)
  (eshell "new")
  (rename-buffer (concat "*eshell: " name "*"))
  (cd (projectile-project-root))
  (rcoedo/eshell/clear)
  (insert "ls")
  (eshell-send-input))

(defun rcoedo/eshell/x ()
  (kill-buffer-and-window))

(req-package evil
  :require general
  :config
  (evil-mode t)

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
    (evil-set-initial-state (nth 0 mode) (nth 1 mode)))

  (nmap "\C-p" nil
        "<tab>" 'other-window)

  (imap "\C-a" 'beginning-of-line
        "\C-e" 'end-of-line
        "\C-f" 'forward-char
        "\C-b" 'backward-char
        "\C-d" 'delete-char
        "\C-n" 'next-line
        "\C-p" 'previous-line
        "\C-w" 'evil-delete
        "\C-k" 'kill-line)

  (mmap "<right>" nil
        "<left>"  nil
        "<down>"  nil
        "<up>"    nil))

(req-package evil-surround
  :require evil
  :config
  (global-evil-surround-mode t)
  (define-key evil-visual-state-map "s" 'evil-surround-region)
  (define-key evil-normal-state-map "s" 'evil-surround-edit))

(req-package evil-search-highlight-persist
  :require evil
  :config
  (global-evil-search-highlight-persist t)
  (define-key evil-normal-state-map (kbd "<backspace>") 'evil-search-highlight-persist-remove-all)
  (custom-set-faces '(evil-search-highlight-persist-highlight-face ((t (:foreground "white" :background "#718c00"))))))

(req-package evil-matchit
  :require evil
  :config
  (progn
    (global-evil-matchit-mode t)))

(req-package evil-lisp-state
  :init
  (progn
    (setq evil-lisp-state-global t
          evil-lisp-state-enter-lisp-state-on-command nil))
  :config
  (progn
    (add-to-list 'evil-lisp-state-major-modes 'clojure-mode)
    (define-key evil-lisp-state-map (kbd "o") 'lisp-state-insert-sexp-after)
    (define-key evil-lisp-state-map (kbd "O") 'lisp-state-insert-sexp-before)
    (evil-lisp-state-leader "L")))

(req-package evil-org
  :init
  (progn
    (add-hook 'org-mode-hook 'evil-org-mode)))

(req-package general
  :config
  (progn
    (general-evil-setup t t)

    (general-create-definer rcoedo--leader-key
                            :states '(normal insert emacs)
                            :prefix "SPC"
                            :non-normal-prefix "C-SPC")

    (general-create-definer rcoedo--mode-key
                            :states '(normal insert emacs)
                            :prefix "<SPC>m"
                            :non-normal-prefix "<C-SPC>m")

    (general-define-key :keymaps 'key-translation-map
                        "C-," (kbd "C-x")
                        "C-." (kbd "C-c")
                        "\e"  (kbd "C-g"))

    (general-define-key "M-]" 'rcoedo/buffer/next-non-emacs-buffer
                        "M-[" 'rcoedo/buffer/previous-non-emacs-buffer
                        "M-q" 'rcoedo/eshell/projectile-eshell-popup
                        "M-i" 'yas-insert-snippet
                        "M-s" 'helm-projectile-ag
                        "M-t" 'helm-projectile-find-file
                        "M-p" 'helm-ghq-list
                        "M-b" 'helm-mini
                        "M-/" 'evilnc-comment-or-uncomment-lines
                        "H-1" 'windmove-left
                        "H-2" 'windmove-down
                        "H-3" 'windmove-up
                        "H-4" 'windmove-right
                        "\C-x2" (lambda () (interactive)(split-window-below) (other-window 1))
                        "\C-x3" (lambda () (interactive)(split-window-right) (other-window 1)))

    (rcoedo--leader-key "jr"    'jump-to-register
                        "jd"    'dired-jump
                        "yy"    'helm-show-kill-ring

                        "cc"    'evilnc-comment-or-uncomment-lines
                        "cp"    'evilnc-copy-and-comment-lines
                        "cb"    'evilnc-comment-or-uncomment-paragraphs
                        "co"    'evilnc-comment-operator

                        "bK"    'kill-buffer-and-window
                        "bk"    'kill-this-buffer
                        "bd"    'rcoedo/buffer/delete-current-file
                        "br"    'rcoedo/buffer/rename-current-file

                        "fs"    'helm-projectile-ag
                        "ft"    'helm-projectile-find-file
                        "ff"    'helm-find-files
                        "fp"    'helm-ghq-list
                        "fb"    'helm-mini)))

(req-package projectile
  :require ghq
  :config
  (progn (setq projectile-enable-caching t
               projectile-switch-project-action 'projectile-dired
               projectile-ignored-project-function #'(lambda (project-root) 'true)
               projectile-completion-system 'helm
               projectile-globally-ignored-directories (append '(".cask") projectile-globally-ignored-files)
               projectile-project-root-files ())
         (projectile-global-mode)))

(req-package ivy
  :config
  (progn (ivy-mode 1)
         (setq ivy-use-virtual-buffers t
               ivy-height 25)))

(req-package helm-config)

(req-package helm
  :require helm-config
  :config
  (setq helm-exit-idle-delay 0)
  (helm-autoresize-mode t)
  (helm-mode t)

  (define-key global-map (kbd "C-x C-f")    'helm-find-files)
  (define-key global-map (kbd "M-x")        'helm-M-x)
  (define-key helm-map   (kbd "<tab>")      'helm-execute-persistent-action)
  (define-key helm-map   (kbd "C-i")        'helm-execute-persistent-action)
  (define-key helm-map   (kbd "C-z")        'helm-select-action)

  (define-key helm-buffer-map     (kbd "<C-backspace>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (lambda (buffer) (kill-buffer buffer) (helm-mini))))))

  (define-key helm-buffer-map     (kbd "<C-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (rcoedo/window/call-other 'switch-to-buffer 'right)))))

  (define-key helm-buffer-map     (kbd "<C-S-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (rcoedo/window/call-other 'switch-to-buffer 'below)))))

  (define-key helm-find-files-map (kbd "<C-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (rcoedo/window/call-other 'find-file 'right)))))

  (define-key helm-find-files-map (kbd "<C-S-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (rcoedo/window/call-other 'find-file 'below))))))

(req-package helm-projectile
  :require projectile helm grep
  :config
  (helm-projectile-toggle 1)
  (setq projectile-switch-project-action 'projectile-dired) ;; Override helm-projectile-on setting
  (define-key projectile-command-map (kbd "s s") 'helm-projectile-ag)
  (define-key projectile-command-map (kbd "p") 'helm-ghq-list)
  (define-key helm-projectile-find-file-map (kbd "<C-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (rcoedo/window/call-other 'find-file 'right)))))
  (define-key helm-projectile-find-file-map (kbd "<C-S-return>")
    #'(lambda () (interactive) (with-helm-alive-p (helm-exit-and-execute-action (rcoedo/window/call-other 'find-file 'below))))))

(req-package helm-ag
  :config
  (define-key helm-ag-map   (kbd "<C-return>")
    #'(lambda ()
        (interactive)
        (with-helm-alive-p (helm-exit-and-execute-action (rcoedo/window/call-other 'helm-ag--action-find-file 'right)))))

  (define-key helm-ag-map   (kbd "<C-S-return>")
    #'(lambda ()
        (interactive)
        (with-helm-alive-p (helm-exit-and-execute-action (rcoedo/window/call-other 'helm-ag--action-find-file 'below))))))

(req-package helm-dash
  :require helm
  :config
  (progn
    (defun rcoedo/helm-dash/setup-docsets (hook docsets)
      (add-hook hook `(lambda ()
                        (setq-local helm-dash-common-docsets ',docsets)
                        (setq helm-current-buffer (current-buffer)))))

    (define-key global-map (kbd "M-d") 'helm-dash-at-point)
    (define-key global-map (kbd "M-D") 'helm-dash)
    (setq helm-dash-browser-func 'eww
          helm-dash-docsets-path "~/.emacs.d/docsets"
          helm-dash-common-docsets (sort
                                    (let (value)
                                    (dolist (element
                                      (directory-files helm-dash-docsets-path nil "\\.docset$" 1)
                                               value)
                                        (setq value (cons (file-name-sans-extension element) value))))
                                    'string-lessp))))

(req-package yasnippet
    :init
    (progn
      (defun rcoedo/yasnippet/bindings ()
        (define-key yas-minor-mode-map (kbd "<tab>") nil)
        (define-key yas-minor-mode-map (kbd "TAB") nil)
        (define-key yas-minor-mode-map (kbd "<C-return>") 'yas-expand))

      (defun rcoedo/yasnippet/hook ()
        (rcoedo/yasnippet/bindings))

      (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))

      (add-hook 'yas-minor-mode-hook 'rcoedo/yasnippet/hook))
    :config
    (yas-global-mode t))

(req-package expand-region
  :require evil
  :config
  (progn
    (define-key evil-normal-state-map "-" 'er/expand-region)))

(req-package eval-in-repl
  :defer t)

(req-package ghq)

(req-package markdown-mode
  :mode "\\.md\\'")

(req-package comint
  :defer t
  :require evil
  :config
  (add-hook 'comint-mode-hook
            #'(lambda ()
                (evil-define-key 'insert comint-mode-map
                  (kbd "C-r") 'helm-comint-input-ring
                  (kbd "C-p") 'comint-previous-input
                  (kbd "C-n") 'comint-next-input))))

(req-package company
  :defer t
  :require helm-company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-hook 'global-company-mode-hook
            #'(lambda ()
                (setq company-idle-delay 0
                      company-dabbrev-downcase nil)
                (define-key company-active-map (kbd "M-n") nil)
                (define-key company-active-map (kbd "M-p") nil)
                (define-key company-active-map (kbd "\C-n") 'company-select-next)
                (define-key company-active-map (kbd "\C-p") 'company-select-previous))))

(req-package eshell
  :defer t
  :require evil
  :config
  (setq eshell-history-size 1000
        eshell-aliases-file (concat user-emacs-directory "eshell-aliases")
        eshell-prompt-function #'(lambda nil (concat (getenv "USER") "@" (system-name) ":"
                                                     (abbreviate-file-name (eshell/pwd))
                                                     (if (= (user-uid) 0) " # " " $ "))))

  (add-hook 'eshell-mode-hook #'(lambda ()
                                  (evil-define-key 'insert eshell-mode-map
                                    (kbd "C-a") 'rcoedo/eshell/maybe-bol
                                    (kbd "C-r") 'helm-eshell-history
                                    (kbd "C-p") 'eshell-previous-matching-input-from-input
                                    (kbd "C-n") 'eshell-next-matching-input-from-input)

                                  (company-mode -1)

                                  (defalias 'ff 'find-file)
                                  (defalias 'd  'dired))))

(req-package flycheck
  :defer t
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(req-package elixir-mode
  :defer t
  :require smartparens
  :config
  (setq blink-matching-delay 0.1)

  (add-hook 'elixir-mode-hook 'alchemist-mode)

  (defun my-elixir-do-end-close-action (id action context)
    (when (eq action 'insert)
      (newline-and-indent)
      (forward-line -1)
      (indent-according-to-mode)))

  (sp-with-modes '(elixir-mode)
    (sp-local-pair "->" "end"
                   :when '(("RET"))
                   :post-handlers '(:add my-elixir-do-end-close-action)
                   :actions '(insert)))

  (sp-with-modes '(elixir-mode)
    (sp-local-pair "do" "end"
                   :when '(("SPC" "RET"))
                   :post-handlers '(:add my-elixir-do-end-close-action)
                   :actions '(insert))))

(req-package smartparens-config
  :config
  (smartparens-global-mode))

(req-package tex-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.tex$\\'" . latex-mode))
  :config
  (add-hook 'latex-mode-hook
            #'(lambda ()
                (flyspell-mode t)
                (ispell-change-dictionary "english")
                (local-unset-key (kbd "C-SPC")))))

(req-package elm-mode
  :defer t
  :require flycheck company
  :init
  (add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))
  :config
  (progn
    (setq elm-format-on-save t)
    (add-to-list 'company-backends 'company-elm)
    (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
    (add-hook 'flycheck-mode-hook 'flycheck-elm-setup)))

(req-package lua-mode
  :defer t
  :require flycheck
  :init
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(req-package guide-key
  :config
  (setq guide-key/guide-key-sequence '("<SPC>" "<C-SPC>" "C-c")
        guide-key/recursive-key-sequence-flag t
        guide-key/idle-delay 0.1
        guide-key/popup-window-position 'bottom)
  (guide-key-mode t))

(req-package cider-repl
  :defer t
  :init
  (progn
    (defun rcoedo/cider-repl/helm-cider-history ()
      "Show `cider-input-history` in `helm`."
      (interactive)
      (helm :sources (helm-build-sync-source "Helm Cider History"
                       :candidates cider-repl-input-history
                       :action '(("Yank" . (lambda (candidate) (insert candidate))))
                       :persistent-action (lambda (candidate) (ignore))
                       :persistent-help "DoNothing"
                       :multiline t)
            :buffer "*helm cider history*"
            :resume 'noresume))

    (defun rcoedo/cider-repl/bindings ()
      (define-key cider-repl-mode-map (kbd "M-p") nil)
      (define-key cider-repl-mode-map (kbd "M-n") nil)
      (define-key cider-repl-mode-map (kbd "M-r") nil)

      (evil-define-key 'insert cider-repl-mode-map
        (kbd "C-r") 'rcoedo/cider-repl/helm-cider-history
        (kbd "C-p") 'cider-repl-previous-input
        (kbd "C-n") 'cider-repl-next-input))

    (defun rcoedo/cider-repl/hook ()
      (setq cider-cljs-lein-repl
               "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
      (rcoedo/cider-repl/bindings))

    (add-hook 'cider-repl-mode-hook 'rcoedo/cider-repl/hook)))

(req-package cider
  :defer t
  :init
  (progn
    (defun rcoedo/cider/bindings ()
      (define-key cider-mode-map (kbd "M-v") 'cider-eval-defun-at-point))

    (defun rcoedo/cider/hook ()
      (eldoc-mode t)
      (rcoedo/cider/bindings))

    (add-hook 'cider-mode-hook 'rcoedo/cider/hook)

    (defun rcoedo/cider/figwheel-repl ()
      (interactive)
      (save-some-buffers)
      (with-current-buffer (cider-current-repl-buffer)
        (goto-char (point-max))
        (insert "(require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
           (figwheel-sidecar.repl-api/cljs-repl)")
        (cider-repl-return)))))

(req-package clojure-mode
  :require helm-dash
  :mode "\\.clj\\'"
  :config
  (progn
    (rcoedo/helm-dash/setup-docsets 'clojure-mode-hook '("Clojure"))
    (defun rcoedo/clojure-mode/hook ()
      (rainbow-delimiters-mode t))

    (add-hook 'clojure-mode-hook 'rcoedo/clojure-mode/hook)))

(req-package anaconda-mode
  :require company eval-in-repl-python
  :init
  (progn
    (defun rcoedo/anaconda-mode/bindings ()
      (define-key python-mode-map (kbd "M-v") 'eir-eval-in-python))

    (defun rcoedo/anaconda-mode/hook ()
      (pyenv-mode t)
      (anaconda-mode t)
      (eldoc-mode t)
      (rcoedo/anaconda-mode/bindings))

    (add-to-list 'company-backends 'company-anaconda)
    (add-hook 'python-mode-hook 'rcoedo/anaconda-mode/hook)))

(req-package
  :mode "\\.hs\\'"
  :commands haskell-mode
  :init
  (progn
    (defun rcoedo/haskell/bindings ()
      (eval-after-load 'haskell-mode '(progn
                                        (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                        (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                        (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                        (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                        (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                        (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
      (eval-after-load 'haskell-cabal '(progn
                                         (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                         (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                         (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                         (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal))))

    (defun rcoedo/haskell/hook ()
      (interactive-haskell-mode 1)
      (hindent-mode 1)
      (custom-set-variables
       '(haskell-process-suggest-remove-import-lines t)
       '(haskell-process-auto-import-loaded-modules t)
       '(haskell-process-log t)
       '(haskell-process-type 'cabal-repl))
      (rcoedo/haskell/bindings))

    (add-hook 'haskell-mode-hook 'rcoedo/haskell/hook)))

(req-package prettier-js
  :init
  (progn
    (setq prettier-js--prettier-args '("--trailing-comma" "all" "--single-quote" "false" "--print-width" "120")
          prettier-target-mode "web-mode")))

(req-package web-mode
  :defer t
  :require flycheck prettier-js
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'"     . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"   . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'"  . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'"    . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'"       . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'"      . web-mode))
  (add-to-list 'auto-mode-alist '("\\.eex\\'"       . web-mode))
  :config
  (progn
    (setq-default flycheck-disabled-checkers
                   (append flycheck-disabled-checkers '(javascript-jshint)))

    (setq web-mode-content-types-alist
          '(("jsx" . "\\.js[x]?\\'")))

    (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
    (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))

    (add-hook 'web-mode-hook (lambda () (rcoedo/enable-minor-mode '("\\.jsx?\\'" . prettier-mode))))
    (add-hook 'web-mode-hook
              #'(lambda ()
                  (setq web-mode-auto-quote-style nil
                        web-mode-enable-auto-pairing nil
                        web-mode-enable-current-column-highlight t
                        web-mode-enable-current-element-highlight t
                        web-mode-attr-indent-offset 2
                        web-mode-markup-indent-offset 2
                        web-mode-css-indent-offset 2
                        web-mode-code-indent-offset 2)

                  (sp-with-modes '(web-mode)
                                 (sp-local-pair "<" nil :actions :rem)
                                 (sp-local-pair "<%" " %>" :wrap "C-%"))
                  ))))

(req-package css-mode
  :mode "\\.css\\'"
  :require general helm-css-scss
  :config
  (progn
    (setq css-indent-offset 2)
    (rcoedo--mode-key :keymaps 'css-mode-map
                      "f" 'helm-css-scss)))

(req-package scss-mode
  :mode "\\.scss\\'"
  :require general helm-css-scss
  :config
  (progn
    (setq scss-compile-at-save nil
          css-indent-offset 2)
    (rcoedo--mode-key :keymaps 'css-mode-map
                      "f" 'helm-css-scss)))

(req-package rainbow
  :defer t
  :init
  (progn
    (setq rainbow-html-colors-major-mode-list '(css-mode
                                                html-mode
                                                scss-mode
                                                web-mode))
    (dolist (mode rainbow-html-colors-major-mode-list)
      (add-hook (intern (format "%s-hook" mode)) 'rainbow-mode))))

(req-package emmet-mode
  :defer t
  :require web-mode
  :init
  (progn
    (defun rcoedo/emmet-mode/bindings ()
      (define-key emmet-mode-keymap (kbd "<C-return>") nil))

    (add-hook 'less-css-mode 'emmet-mode)
    (add-hook 'scss-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode))
    (add-hook 'emmet-mode-hook 'rcoedo/emmet-mode/bindings))

(req-package magit
  :bind (("C-c g s"   . magit-status)
         ("C-c g l l" . magit-log)
         ("C-c g l c" . magit-log-current)))

(req-package popwin
  :config
  (popwin-mode 1))

(req-package org
  :config
  (progn
    (rcoedo--mode-key :keymaps 'org-mode-map
                      "t" 'org-babel-tangle)
    (define-key org-mode-map (kbd "<S-right>") nil)
    (define-key org-mode-map (kbd "<S-left>")  nil)
    (define-key org-mode-map (kbd "C-'")       nil)))

(req-package transpose-frame
  :config
  (define-key global-map (kbd "M-f") 'flip-frame)
  (define-key global-map (kbd "M-F") 'flop-frame)
  (define-key global-map (kbd "M-r") 'rotate-frame-clockwise)
  (define-key global-map (kbd "M-R") 'rotate-frame-anticlockwise))

(req-package lisp-mode
  :init
  (progn
    (defun rcoedo/lisp-mode/bindings ()
      (define-key emacs-lisp-mode-map (kbd "M-v") 'eval-defun))

    (defun rcoedo/lisp-mode/hook ()
      (rcoedo/lisp-mode/bindings)
      (rainbow-delimiters-mode t))

    (add-hook 'emacs-lisp-mode-hook 'rcoedo/lisp-mode/hook)))

(req-package octave
  :mode ("\\.m$" . octave-mode)
  :init
  (progn
    (defun rcoedo/octave-mode/bindings ()
      (define-key octave-mode-map (kbd "M-v") 'octave-send-defun))

    (defun rcoedo/octave-mode/hook ()
      (rcoedo/octave-mode/bindings))

    (add-hook 'octave-mode-hook 'rcoedo/octave-mode/hook)))

(req-package ess-site
  :disabled t
  :mode ("\\.R$" . R-mode)
  :init
  (progn
    (defun rcoedo/ess-mode/bindings ()
      (define-key ess-mode-map (kbd "M-v") 'ess-eval-paragraph-and-step))

    (defun rcoedo/ess-mode/hook ()
      (rcoedo/ess-mode/bindings))

    (add-hook 'ess-mode-hook 'rcoedo/ess-mode/hook)))

(req-package eww
  :defer t
  :config
  (progn
    (evil-define-key 'normal eww-mode-map
      (kbd "q") 'quit-window)))

(req-package dired
  :defer t
  :config
  (progn
    (define-key dired-mode-map (kbd "M-s") nil)))

(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)                 ; Allow the use of erase-buffer
(windmove-default-keybindings)                    ; Move between windows with shift + arrow keys
(transient-mark-mode t)                           ; Show the mark as selected
(global-auto-revert-mode t)                       ; Reload buffers when they change outside emacs

(setq-default c-basic-offset 4
              truncate-lines nil
              prefer-coding-system 'utf-8
              indent-tabs-mode nil
              global-auto-revert-non-file-buffers t ;; Auto-revert
              auto-revert-verbose nil
              tab-width 4
              backup-inhibited t
              auto-save-default nil
              rcoedo/layout/layout-list '(rcoedo/layout/three rcoedo/layout/four rcoedo/layout/side-by-side rcoedo/layout/bottom-panel))

(eval-after-load 'undo-tree '(progn (define-key undo-tree-map (kbd "C-/") nil)))

(req-package-finish)
