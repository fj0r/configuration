if (( $+commands[dstask])); then
    alias q=dstask
    _dstask() {
        compadd -- $(dstask _completions "${words[@]}")
    }
    compdef _dstask dstask
fi

