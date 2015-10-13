;;; elm.el --- elm layer

;;; Commentary:
;;; Configures elm mode

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package elm-mode
  :ensure t
  :config
  (flycheck-define-checker elm
    "An Elm syntax checker
Uses Elm compiler. See URL
`http://elm-lang.org'."
    :command ("elm"
              "-m"
              "-o"        ; only javascript
              source)
    :error-patterns
    ((error line-start "Parse error at (line " line ", column " column "):\n"
            (message) line-end)
     (error line-start "Error on line " line ", column " column " to " (one-or-more digit) ":\n"
            (message) line-end)
     (error line-start "Type error on line " line ", column " column " to " (one-or-more digit)":\n"
            (message (one-or-more (or not-newline "\n")))
            line-end)
     (error line-start "Type Error: "
            (message (one-or-more (or not-newline "\n")))
            line-end)
     (error line-start "Syntax Error: "
            (message (one-or-more (or not-newline "\n")))))
    :modes (elm-mode))

  (add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))
  (add-to-list 'flycheck-checkers 'elm))

(provide 'elm)
;;; elm.el ends here
