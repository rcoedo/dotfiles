(require 'guide-key)
(setq guide-key/guide-key-sequence '("<SPC>" "C-c")
      guide-key/recursive-key-sequence-flag t
      guide-key/idle-delay 0.1
      guide-key/popup-window-position 'bottom)
(guide-key-mode t)
