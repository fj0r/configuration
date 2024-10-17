$env.LS_ROOT = '/opt/language-server'

#$env.EDITOR = 'nvim --cmd "let g:flatten_wait=1"'

if ($env.EDITOR == 'nuedit') and (not ($'($env.HOME)/.local/bin/nuedit' | path exists)) {
    mkdir $'($env.HOME)/.local/bin/'
    cp $'($nu.default-config-dir)/nuedit' $'($env.HOME)/.local/bin/nuedit'
}

