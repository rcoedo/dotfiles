(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(osx-clipboard-mode t)
(global-linum-mode t)

(show-paren-mode t)

(setq backup-directory-alist `((".*" . "~/.saves"))
      auto-save-file-name-transforms '((".*" "~/.saves/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
