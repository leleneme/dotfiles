# Only display a unicode face when not running in a tty
if [[ $(tty) == /dev/tty* ]]; then
    __FACE=""
else
    __FACE="๑(◕‿◕)๑ "
fi

_PROMPT_COLOR="\e[0;36m"
RESET="\[`tput sgr0`\]"
GREEN="\[`tput setaf 108`\]"
BLUE1="\[`tput setaf 111`\]"
BLUE2="\[`tput setaf 85`\]"

PS1="\n${BLUE1}\u@\h ${GREEN}\w ${BLUE2}\$(get_git_branch)\n${RESET}$ "

__MAGENTA="\e[0;95m"
__GREEN="\e[0;92m"
__WELCOME_MSG="${__MAGENTA}${__FACE}${_PROMPT_COLOR}logged as ${__MAGENTA}${USER}${_PROMPT_COLOR} at ${__MAGENTA}${HOSTNAME}"
__WELCOME_MSG="${__WELCOME_MSG} ${__MAGENTA}${_PROMPT_COLOR}|${__GREEN} $(uptime -p)"

echo -e "\r${__WELCOME_MSG}\e[m"
