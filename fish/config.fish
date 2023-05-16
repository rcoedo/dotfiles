export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
export EDITOR=nvim
export GIT_EDITOR=$EDITOR
export TIG_EDITOR=nvim
export GREP_OPTIONS="--color=auto"
export HOMEBREW_NO_ANALYTICS=1
export HOMEBREW_NO_GITHUB_API=1
export HOMEBREW_NO_ENV_HINTS=1
export RIPGREP_CONFIG_PATH=$HOME/.config/ripgrep/ripgreprc

export FZF_DEFAULT_OPTS="
  --color=fg:#c0caf5,bg:#1a1b26,hl:#bb9af7 \
  --color=fg+:#c0caf5,bg+:#292e42,hl+:#7dcfff \
  --color=info:#7aa2f7,prompt:#7dcfff,pointer:#7dcfff \
  --color=marker:#9ece6a,spinner:#9ece6a,header:#9ece6a \
  -x \
  -m \
  --ansi \
  --reverse \
  --inline-info"

fish_add_path /opt/homebrew/bin

set -x GPG_TTY (tty)
set -gx PATH $PATH

# Project
abbr p --function __project_manager
abbr r --function __project_run

# Other abreviatures
abbr hm "history --merge"
abbr g git
abbr dc "docker compose"
abbr d docker
abbr mk "mkdir -p"
abbr rf "rm -rf"
abbr y yarn
abbr bb "cd $GHQ_ROOT/bitbucket.org/rcoedo"
abbr gh "cd $GHQ_ROOT/github.com/rcoedo"
abbr gl "cd $GHQ_ROOT/gitlab.com/rcoedo"
abbr ag rg
abbr find fd
abbr f fork
abbr rr ranger
abbr w webstorm
abbr jk jekyll
abbr tree "exa --tree --git-ignore"
abbr ef "exec fish"
abbr ls exa
abbr ll "exa -la"
abbr lkc "ssh-add --apple-load-keychain"
abbr lg lazygit
abbr ld lazydocker
abbr t tig
abbr cf "vim ~/.config/fish/config.fish"
abbr n deno
abbr nt "deno task"

alias vim nvim
alias v "neovide --frame none"
alias gg __ghq_get
alias bat "bat --theme=base16"
alias cat "bat --theme=base16 --paging=never"
alias ncdu "ncdu --color dark -rr -x"
alias yd "ydiff -s"
alias https "http --default-scheme=https --verify=no"
