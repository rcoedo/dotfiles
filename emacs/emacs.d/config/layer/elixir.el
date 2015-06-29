;;; elixir.el --- elixir layer

;;; Commentary:
;;; Configures elixir and alchemist

;;; Code:
(eval-when-compile
  (require 'use-package))

(use-package elixir-mode
  ;;; Load elixir layer
  :ensure t
  :ensure alchemist
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode)
  (evil-leader/set-key-for-mode 'elixir-mode
    "mcb" 'alchemist-compile-this-buffer
    "mel" 'alchemist-eval-current-line
    "meL" 'alchemist-eval-print-current-line
    "mer" 'alchemist-eval-region
    "meR" 'alchemist-eval-print-region
    "meb" 'alchemist-eval-buffer
    "meB" 'alchemist-eval-print-buffer

    "mgt" 'alchemist-project-open-tests-for-current-file

    "mh:" 'alchemist-help
    "mhH" 'alchemist-help-history
    "mhh" 'alchemist-help-search-at-point
    "mhr" 'alchemist-help-search-marked-region

    "mm:" 'alchemist-mix
    "mmc" 'alchemist-mix-compile
    "mmx" 'alchemist-mix-run
    "mmh" 'alchemist-mix-help
    "mmi" 'alchemist-iex-project-run

    "msi" 'alchemist-iex-run
    "msl" 'alchemist-iex-send-current-line
    "msL" 'alchemist-iex-send-current-line-and-go
    "msr" 'alchemist-iex-send-region
    "msR" 'alchemist-iex-send-region-and-go

    "mta" 'alchemist-mix-test
    "mtb" 'alchemist-mix-test-this-buffer
    "mtt" 'alchemist-mix-test-at-point

    "mxb" 'alchemist-execute-this-buffer
    "mxf" 'alchemist-execute-file
    "mx:" 'alchemist-execute
    ))

(provide 'elixir)
;;; elixir.el ends here
