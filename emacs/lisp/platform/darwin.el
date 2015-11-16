(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq mac-command-modifier 'meta
      mac-option-key-is-meta nil
      mac-option-modifier 'none
      system-uses-terminfo nil
      osx-clipboard-mode t)
