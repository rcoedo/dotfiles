;;; evil-settings.el --- Evil configuration
;;; Commentary:
;;; Code:

;; Evil mode
(require 'req-package)

(req-package evil
  :config
  (evil-mode t)

  (defvar evil-mode-list
    '((eshell-mode         insert)
      (comint-mode         insert)
      (alchemist-iex-mode  insert)
      (magit-mode          emacs)
      (magit-status        emacs)
      (magit-log-mode      emacs)
      (magit-commit-mode   emacs)
      (magit-diff-mode     emacs)
      (magit-popup-mode    emacs)
      (magit-merge-popup   emacs)
      (magit-revision-mode emacs)
      (direx:direx-mode    emacs)
      (git-commit-mode     insert)))

  (dolist (mode evil-mode-list)
    (evil-set-initial-state (nth 0 mode) (nth 1 mode)))

  (define-key evil-insert-state-map "\C-a" 'beginning-of-line)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-insert-state-map "\C-f" 'forward-char)
  (define-key evil-insert-state-map "\C-b" 'backward-char)
  (define-key evil-insert-state-map "\C-d" 'delete-char)
  (define-key evil-insert-state-map "\C-n" 'next-line)
  (define-key evil-insert-state-map "\C-p" 'previous-line)
  (define-key evil-insert-state-map "\C-w" 'evil-delete)
  (define-key evil-insert-state-map "\C-k" 'kill-line))

(req-package evil-leader
  :require evil
  :config
  (global-evil-leader-mode)
  (setq evil-leader/in-all-states t)

  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "yy"    'helm-show-kill-ring
    "cc"    'evilnc-comment-or-uncomment-lines
    "cp"    'evilnc-copy-and-comment-lines
    "cb"    'evilnc-comment-or-uncomment-paragraphs
    "co"    'evilnc-comment-operator
    "sh"    'evil-search-highlight-persist-remove-all
    "TAB"   'transpose-frame))

(req-package evil-surround
  :require evil
  :config
  (global-evil-surround-mode t))

(req-package evil-search-highlight-persist
  :require evil
  :config
  (global-evil-search-highlight-persist t))

(provide 'evi-settingsl)
;;; evil-settings.el ends here
