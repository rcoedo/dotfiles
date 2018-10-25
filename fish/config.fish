export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
export EDITOR="nvim"
export GIT_EDITOR="nvim"
export GREP_OPTIONS="--color=auto"
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_GITHUB_API=1
export BREW_CELLAR="/usr/local/Cellar"
export TOMCATS=$HOME/Workspace/tomcats
export CATALINA_HOME=$TOMCATS/apache-tomcat-8.0.29
export GOPATH=$HOME/Workspace
export YARNPATH=$HOME/.config/yarn
__add_to_path $GOPATH/bin

abbr hm "history --merge"
abbr g "git"
abbr dc "docker-compose"
abbr kb "kubectl"
abbr d "docker"
abbr mk "mkdir -p"
abbr b "bundle"
abbr be "bundle exec"
abbr note "jupyter notebook"
abbr dja "django-admin"
abbr dj "python manage.py"
abbr y "yarn"
abbr ll "ls -lash"

abbr t "tig"
abbr ts "tig status"
abbr tl "tig log"

alias vim "nvim"
alias wtr "curl -4 wttr.in"
alias tx "tmuxinator"
alias bb "cd $GHQ_ROOT/bitbucket.org/rcoedo"
alias gh "cd $GHQ_ROOT/github.com/rcoedo"
alias gl "cd $GHQ_ROOT/gitlab.com/rcoedo"
alias miex "iex -S mix"
alias r "ranger"
alias ta "tmux attach-session"
alias gg "__ghq_get"
alias ibrew "__fuzzy_brew"
alias icask "__fuzzy_cask"
alias ik "__fuzzy_kill"
alias icd "__fuzzy_cd"
alias dr "__docker_run_in_dir"
alias tree "tree -C -I 'node_modules'"
alias dcpg "wget https://gist.github.com/rcoedo/63cc1f7c3af12fb9c0a38e2e10843ea7/raw/ -O docker-compose.yml"
alias bat "bat --theme=zenburn"
alias du "ncdu --color dark -rr -x"
alias ping "prettyping"
alias yd "ydiff -s"
alias db "nodevtools"
