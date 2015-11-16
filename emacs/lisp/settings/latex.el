(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))

(defun latex-fun-hook ()
  (flyspell-mode t)
  (ispell-change-dictionary "espanol")
  (local-set-key (kbd "C-SPC") 'helm-mini))

(add-hook 'latex-mode-hook 'latex-fun-hook)
