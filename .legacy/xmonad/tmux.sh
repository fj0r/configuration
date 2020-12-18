#!/bin/zsh
export TOPIC="${1-master}"
export DIR="$2"

if [[ ! -r "$HOME/.ssh/ssh-agent.socket" ]]; then 
        eval `ssh-agent -a $HOME/.ssh/ssh-agent.socket` 
fi 
export SSH_AUTH_SOCK="$HOME/.ssh/ssh-agent.socket"


if [[ -z "$TOPIC" ]]; then
        export TOPIC="master"
fi
echo "I belong to $TOPIC" 1>&2
SES_NAME="$(openssl rand -hex 3)"
CLIENTS=$(tmux list-clients)
echo "$CLIENTS"

if [[ -z "$CLIENTS" ]]; then
   tmux attach -t "master" || tmux new-session -s "master" \; set-environment TOPIC "$TOPIC"

else

    tmux has-session -t "master"  > /dev/null 2>&1
if [[ "$?" == "1" ]]; then
    tmux new-session -s "master" \; set-environment TOPIC "$TOPIC"
else
    sesname=$(echo "${TOPIC}-${SES_NAME}" | sed 's/|/-/')
    tmux new-session -t "master" -s "${sesname}"  \; new-window \; set-environment TOPIC "$TOPIC"
fi
fi
