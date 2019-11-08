#!/usr/bin/env sh
tree -C -I $((cat .gitignore 2> /dev/null || cat $(git rev-parse --show-toplevel 2> /dev/null)/.gitignore 2> /dev/null || echo "node_modules") | egrep -v "^#.*$|^[[:space:]]*$" | tr "\\n" "|" | rev | cut -c 2- | rev)
