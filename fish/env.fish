#set -x SLASH_HOME $HOME/.slash
#__add_to_path $SLASH_HOME/bin

#set -x JAVA_HOME (/usr/libexec/java_home)
#__add_to_path $JAVA_HOME/bin

#set -x M2_REPO $HOME/.m2/repository
#set -x M2_HOME $SLASH_HOME/apache-maven-3.2.3
#__add_to_path $M2_HOME/bin
#__add_to_path $M2_REPO

#set -x ANDROID_HOME $SLASH_HOME/android-sdk-macosx

#set -gx RBENV_ROOT /usr/local/var/rbenv
#__add_to_path $HOME/.rbenv/bin
#status --is-interactive; and . (rbenv init -|psub)

#set -x DOCKER_HOST tcp://192.168.200.2:2375
#set -x OCH_MEMORY 2048

#set -x NDENV_ROOT $HOME/.ndenv
#__add_to_path $NDENV_ROOT/bin
#__add_to_path $NDENV_ROOT/shims
#ndenv rehash

#set -x EXENV_ROOT $HOME/.exenv
#__add_to_path $EXENV_ROOT/bin
#__add_to_path $EXENV_ROOT/shims
#exenv rehash

#set -x PYENV_ROOT $HOME/.pyenv
#__add_to_path $PYENV_ROOT/bin
#__add_to_path $PYENV_ROOT/shims
#pyenv rehash

#set -x GOPATH $HOME/Workspace/go
#__add_to_path $GOPATH/bin

#eval (direnv hook fish)
