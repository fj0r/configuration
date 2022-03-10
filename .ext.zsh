if [[ ! "$PATH" == */opt/bin* && -d /opt/bin ]]; then
    export PATH=/opt/bin:$PATH
fi


# sources $CFG/.fzf/init.zsh

sources $CFG/.dstask.zsh
sources $CFG/.xkb.zsh
sources $CFG/../.user.zsh
sources $HOME/.test.zsh
sources $CFG/../.mutagen-alias

