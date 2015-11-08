;;; evil.el --- Evil configuration
;;; Commentary:
;;; Code:

;; Evil mode
(require 'evil)
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
(define-key evil-insert-state-map "\C-k" 'kill-line)

;; Evil leader
(require 'evil-leader)
(global-evil-leader-mode)

(setq evil-leader/in-all-states t)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "<SPC>" 'avy-goto-word-1

  "yy"    'helm-show-kill-ring

  "cc"    'evilnc-comment-or-uncomment-lines
  "cp"    'evilnc-copy-and-comment-lines
  "cb"    'evilnc-comment-or-uncomment-paragraphs
  "co"    'evilnc-comment-operator

  "E"     'eshell

  "sh"    'evil-search-highlight-persist-remove-all

  "TAB"   'transpose-frame

  "i"     'package-install)

;; Evil surround
(require 'evil-surround)
(global-evil-surround-mode t)

;; Evil highlight
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

(provide 'evil)
;;; evil.el ends here
