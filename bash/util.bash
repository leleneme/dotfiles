alias rm='trash' # Don't forget 20.Feb.2021
alias ls='ls --color=auto'
alias ll='ls --color=auto -l'
alias grep='grep --color=auto'

alias em='emacsclient -c -a ""'
alias emd='em & disown'

alias sctl='systemctl'
alias uctl='systemctl --user'

if ! [ -x "$(command -v doas)" ]; then
    # doas is not installed, so we alias it to sudo
    alias doas='sudo'
fi

function set_nvfan() {
    doas nvidia-settings --display $DISPLAY -a GPUFanControlState=1 -a GPUTargetFanSpeed="$1"
}

function unset_nvfan() {
    doas nvidia-settings --display $DISPLAY -a GPUFanControlState=0
}

function get_git_branch() {
    BRANCH=$(git branch --show-current 2> /dev/null)
    [[ -z $BRANCH ]] || echo "($BRANCH)" && echo ""
}

function mcd() {
    mkdir -p "$1" && cd "$1"
}

function crop_video() {
    if [ $# -eq 0 ]; then
	echo "Usage: crop_video <start> <end> <source> <output>"
    else
	ffmpeg -ss $1 -to $2 -i "$3" "$4"
    fi
}

function calc() {
    echo "$@" | bc -l
}

