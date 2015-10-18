;;; evil.el --- Evil configuration
;;; Commentary:
;;; Code:

;; Evil mode
(require 'evil)
(evil-mode t)

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

;; Evil magit stuff
(evil-set-initial-state 'magit-mode 'emacs)
(evil-set-initial-state 'magit-status-mode 'emacs)
(evil-set-initial-state 'magit-log-mode 'emacs)
(evil-set-initial-state 'magit-commit-mode 'emacs)
(evil-set-initial-state 'magit-diff-mode 'emacs)
(evil-set-initial-state 'magit-popup-mode 'emacs)
(evil-set-initial-state 'magit-merge-popup 'emacs)
(evil-set-initial-state 'magit-revision-mode 'emacs)
(evil-set-initial-state 'git-commit-mode 'insert)

(provide 'evil)
;;; evil.el ends here
