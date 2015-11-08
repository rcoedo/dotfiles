(load "functions/eshell.el")

;; Eshell key definitions for evil insert mode
(evil-define-key 'insert eshell-mode-map
  (kbd "C-a") 'eshell-maybe-bol
  (kbd "C-r") 'helm-eshell-history
  (kbd "C-p") 'eshell-previous-matching-input-from-input
  (kbd "C-n") 'eshell-next-matching-input-from-input)

(defalias 'ff 'find-file)
(defalias 'd  'dired)

;; Eshell-prompt
(setq eshell-prompt-function
      #'(lambda nil (concat (getenv "USER") "@" (system-name) ":"
           (abbreviate-file-name (eshell/pwd))
           (if (= (user-uid) 0) " # " " $ "))))

(setq eshell-history-size 1000 
      eshell-banner-message (format "%s %s\nwith Emacs %s on %s"
                                    (propertize "Eshell session started on" 'face '((:foreground "Goldenrod")))
                                    (propertize (format-time-string "%c") 'face '((:foreground "magenta")))
                                    (propertize emacs-version 'face '((:foreground "magenta")))
                                    (propertize (with-temp-buffer (call-process "uname" nil t nil "-r")
                                                                  (buffer-string))
                                                'face '((:foreground "magenta")))))

(define-key global-map (kbd "M-e")     'eshell-here)

;; ;; Compatibility 24.2/24.3
;; (unless (fboundp 'eshell-pcomplete)
;;   (defalias 'eshell-pcomplete 'pcomplete))

;; (add-hook 'eshell-mode-hook
;;           #'(lambda ()
;;               (setq eshell-pwd-convert-function (lambda (f)
;;                                                   (if (file-equal-p (file-truename f) "/")
;;                                                       "/" f)))
;;               (setq eshell-cmpl-ignore-case t)
              ;; (eshell-cmpl-initialize)
              ;; (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              ;; (define-key eshell-mode-map [remap eshell-complete-lisp-symbol] 'helm-lisp-completion-at-point)
              ;; (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
              ;; (set-face-attribute 'eshell-prompt nil :foreground "DeepSkyBlue")
              ;; (push-mark)))
