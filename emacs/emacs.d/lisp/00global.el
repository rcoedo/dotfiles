(setq inhibit-splash-screen t)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(menu-bar-mode -1)
(osx-clipboard-mode t)

(global-linum-mode t)
(setq linum-format "%d ")

(setq mac-command-modifier 'meta)

(setq backup-directory-alist `((".*" . "~/.saves"))
      auto-save-file-name-transforms '((".*" "~/.saves/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq system-uses-terminfo nil)
(prefer-coding-system 'utf-8)
(windmove-default-keybindings) 
(setq-default truncate-lines nil)
(setq-default indent-tabs-mode nil)
(setq search-highlight t)
(transient-mark-mode t)
