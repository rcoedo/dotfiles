(require 'guide-key)
(setq guide-key/guide-key-sequence '("<SPC>" "<SPC>f" "<SPC>a" "<SPC>p" "<SPC>c" "C-c a")
      guide-key/idle-delay 0.1
      guide-key/popup-window-position 'bottom)
(guide-key-mode 1)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key

  "ff" 'helm-find-files
  "fd" 'dired

  "pf" 'helm-projectile-find-file
  "pa" 'helm-projectile-ag

  "wh" 'evil-window-left
  "wj" 'evil-window-bottom
  "wk" 'evil-window-top
  "wl" 'evil-window-right

  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "\\" 'evilnc-comment-operator ; if you prefer backslash key

  "jl"  'helm-buffers-list

  "i"  'package-install)
