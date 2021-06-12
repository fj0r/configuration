if [[ ! "$PATH" == */opt/bin* && -d /opt/bin ]]; then
    export PATH=/opt/bin:$PATH
fi

if (( $+commands[dstask])); then
    alias q=dstask
    _dstask() {
        compadd -- $(dstask _completions "${words[@]}")
    }
    compdef _dstask dstask
fi

# sources $CFG/.fzf/init.zsh

sources $CFG/.user.zsh
sources $HOME/.test.zsh
sources $CFG/.mutagen-alias

