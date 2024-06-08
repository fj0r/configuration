### {{{ base.nu
$env.comma_scope = {|_|{ created: '2024-05-13{1}14:06:59' }}
$env.comma = {|_|{}}
### }}}

### {{{ 03_reload.nu
'. reload'
| comma fun {|a,s,_|
    let act = $a | str join ' '
    $', ($act)' | batch -i ',.nu'
} {
    watch: { glob: ",.nu", clear: true }
    completion: {|a,s|
        , -c ...$a
    }
    desc: "reload & run ,.nu"
}
### }}}


'desktop neovide'
| comma fun {
    sudo cp desktop/neovide.desktop /usr/share/applications/neovide.desktop
}

'setup nushell'
| comma fun {|a,s,_|
    ln -fs $"($_.wd)/dotenv.nu" ~/.env.nu
    rm -f ~/.config/nushell
    ln -fs $"($_.wd)/nushell" ~/.config/nushell
    ln -fs $"($_.wd)/dot.nu" ~/.nu
}

'setup nvim'
| comma fun {|a,s,_|
    ln -fs $"($_.wd)/nvim" ~/.config/nvim
}
