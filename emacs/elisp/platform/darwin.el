(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(setq mac-command-modifier 'meta
      system-uses-terminfo nil
      osx-clipboard-mode t)
