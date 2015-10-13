set fish_greeting ""

# Add function subdirectories to the fish function path
set fish_function_path $fish_function_path (find $HOME/.config/fish/functions/* -type d)

set -x EDITOR "vim"
set -x GIT_EDITOR "vim"
set -x GREP_OPTIONS "--color=auto"

source $HOME/.config/fish/env.fish
source $HOME/.config/fish/colors.fish
source $HOME/.config/fish/alias.fish

set -x NO_FISHMARKS_COMPAT_ALIASES
source $HOME/.fishmarks/marks.fish
