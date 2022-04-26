if [[ ! "$PATH" == */opt/bin* && -d /opt/bin ]]; then
    export PATH=/opt/bin:$PATH
fi

for i in $(find /opt/*/bin -maxdepth 0); do
    if [[ ! "$PATH" == *$i* ]]; then
        export PATH=$i:$PATH
    fi
done


# sources $CFG/.fzf/init.zsh

#sources $CFG/.xkb.zsh
sources $CFG/../.dev.zsh
sources $CFG/../.user.zsh
sources $HOME/.test.zsh
sources $CFG/../.mutagen-alias

