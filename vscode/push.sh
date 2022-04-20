#!/bin/sh
cat extensions | xargs -L 1 code --install-extension
cp keybindings.json settings.json ~/Library/Application\ Support/Code/User/
