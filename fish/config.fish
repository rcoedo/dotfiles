export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
export EDITOR="/opt/homebrew/bin/nvim"
export GIT_EDITOR="nvim"
export GREP_OPTIONS="--color=auto"
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_GITHUB_API=1
export HOMEBREW_NO_ENV_HINTS=1

fish_add_path /opt/homebrew/bin

set -x GPG_TTY (tty)
set -gx PATH $PATH

abbr hm "history --merge"
abbr g "git"
abbr dc "docker compose"
abbr d "docker"
abbr mk "mkdir -p"
abbr rf "rm -rf"
abbr y "yarn"
abbr bb "cd $GHQ_ROOT/bitbucket.org/rcoedo"
abbr gh "cd $GHQ_ROOT/github.com/rcoedo"
abbr gl "cd $GHQ_ROOT/gitlab.com/rcoedo"
abbr ag "rg"
abbr find "fd"
abbr f "fork"
abbr r "ranger"
abbr ws "webstorm"
abbr jk "jekyll"
abbr tree "tree --gitignore"

abbr t "tig"
abbr ts "tig status"
abbr tl "tig log"

alias vim "nvim"
alias wtr "curl -4 wttr.in"
alias tx "tmuxinator"
alias miex "iex -S mix"
alias ta "tmux attach-session"
alias gg "__ghq_get -u rcoedo"
alias ibrew "__fuzzy_brew"
alias icask "__fuzzy_cask"
alias ik "__fuzzy_kill"
alias icd "__fuzzy_cd"
alias bat "bat --theme=zenburn"
alias ncdu "ncdu --color dark -rr -x"
alias yd "ydiff -s"
alias db "nodevtools"
alias https "http --default-scheme=https --verify=no"
alias n "nnn -e"
