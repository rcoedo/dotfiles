(menu-bar-mode -1)                                      ;; Hide menu bar
(scroll-bar-mode -1)                                    ;; Hide scroll bar
(tool-bar-mode -1)                                      ;; Hide tool bar
(setq frame-resize-pixelwise t)

(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(internal-border-width . 0))

(provide 'early-init)
