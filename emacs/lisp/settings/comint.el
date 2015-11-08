(evil-define-key 'insert comint-mode-map
  (kbd "C-r") 'helm-comint-input-ring
  (kbd "C-p") 'comint-previous-input
  (kbd "C-n") 'comint-next-input)
