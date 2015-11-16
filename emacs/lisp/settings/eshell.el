(load "functions/eshell.el")
;; Eshell-prompt
(setq eshell-prompt-function
      #'(lambda nil (concat (getenv "USER") "@" (system-name) ":"
           (abbreviate-file-name (eshell/pwd))
           (if (= (user-uid) 0) " # " " $ "))))

(setq eshell-history-size 1000)

(define-key global-map (kbd "M-e") 'eshell-projectile)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (evil-define-key 'insert eshell-mode-map
                (kbd "C-a") 'eshell-maybe-bol
                (kbd "C-r") 'helm-eshell-history
                (kbd "C-p") 'eshell-previous-matching-input-from-input
                (kbd "C-n") 'eshell-next-matching-input-from-input)

              (defalias 'ff 'find-file)
              (defalias 'd  'dired)

              (company-mode -1))) ;; Disable company mode on eshell
