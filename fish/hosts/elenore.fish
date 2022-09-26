export GHQ_ROOT="$HOME/Workspace/src"

export GOPATH=$HOME/Workspace
fish_add_path $GOPATH/bin

export POETRY_PATH=$HOME/.poetry
fish_add_path $POETRY_PATH/bin

export VOLTA_HOME="$HOME/.volta"
fish_add_path $VOLTA_HOME/bin

export CARGO_PATH=$HOME/.cargo
fish_add_path $CARGO_PATH/bin

fish_add_path /opt/homebrew/opt/mysql-client/bin

export PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true
export PUPPETEER_EXECUTABLE_PATH=(which chromium)
