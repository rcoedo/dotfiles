(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq mac-command-modifier 'meta
      mac-option-modifier 'super
      system-uses-terminfo nil
      osx-clipboard-mode t)
