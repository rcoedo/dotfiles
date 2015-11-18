# Emacs configuration

This is my emacs configuration.
It uses Cask as package manager and Org-babel to tangle the org file.

## Dependencies

To install the packages you should have `Cask` installed.

## Installation

1. copy the emacs folder into `~/.emacs.d`
2. cd `~/.emacs.d`
3. run `cask install`
4. open init.org with emacs
5. tangle the org file with `M-x org-babel-tangle` to generate an `init.el` file
6. restart emacs
