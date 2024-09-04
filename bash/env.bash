export HISTSIZE=5000

# default command-line editor 
export EDITOR=micro
# disable dotnet telemetry
export DOTNET_CLI_TELEMETRY_OPTOUT=1

PATH=$PATH:$HOME/.dotnet/tools/
PATH=$PATH:$HOME/go/bin
PATH=$PATH:$HOME/.cargo/bin
PATH=$PATH:$HOME/.local/bin
PATH=$PATH:$HOME/.luarocks/bin
export PATH
