(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

(setq inhibit-splash-screen t
      linum-format 'dynamic
      menu-bar-mode -1)

(global-linum-mode t)
;; Trick to set the right amount of spaces on linum
(defadvice linum-update-window (around linum-dynamic activate)
  "Number into string with enough spaces."
  (let* ((w (length (number-to-string
                     (count-lines (point-min) (point-max)))))
         (linum-format (concat " %" (number-to-string w) "d ")))
    ad-do-it))

(load "interface/modeline.el")

(load-theme 'zenburn t)
(set-frame-font (font-spec :family "Monaco" :size 12))
