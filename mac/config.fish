fish_add_path -a /opt/homebrew/bin
fish_add_path -a $HOME/.cargo/bin
set -x VOLTA_HOME $HOME/.volta
set -x GOPATH $HOME/go
fish_add_path -a $VOLTA_HOME/bin
fish_add_path -a $GOPATH/bin
fish_add_path -a /usr/local/bin
fish_add_path -a /usr/local/go/bin

starship init fish | source