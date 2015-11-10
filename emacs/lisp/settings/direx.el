(require 'direx)

(add-hook 'direx:direx-mode-hook (lambda () (setq mode-line-format nil)
                                   (linum-mode 0)))

(push '(direx:direx-mode :position left :width 40 :dedicated t)
      popwin:special-display-config)

(evil-define-key 'normal direx:direx-mode-map (kbd "TAB") 'direx:toggle-item)

(defun direx:jump-to-projectile-root ()
  (interactive)
  (direx:find-directory-other-window (projectile-project-root)))

;; Projectile mapping
(define-key projectile-command-map (kbd "d") 'direx:jump-to-projectile-root)

(global-set-key (kbd "C-x j") 'direx:jump-to-directory-other-window)
