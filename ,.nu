export def 'desktop neovide' [] {
    cp desktop/neovide.desktop ~/.local/share/applications
    sudo cp desktop/neovide.svg /usr/local/share/icon
    sudo cp desktop/neovide.desktop /usr/share/applications/neovide.desktop
}

export def 'setup nushell' [] {
    let _ = $env.PWD
    ln -fs $"($_)/dotenv.nu" ~/.env.nu
    rm -f ~/.config/nushell
    ln -fs $"($_)/nushell" ~/.config/nushell
    ln -fs $"($_)/dot.nu" ~/.nu
}

export def 'setup nvim' [] {
    ln -fs $"($env.PWD)/nvim" ~/.config/nvim
}

def cmpl-feed [] {
    ls watchList/ | get name
}

export def 'list feeds' [
    file:string@cmpl-feed = 'watchList/feeds.opml'
] {
    open $file
    | from xml
    | get content.content.1
    | reduce -f {} {|x, a|
        $a
        | insert $x.attributes.text (
            $x
            | get content
            | get attributes
            | select title xmlUrl
        )
    }
}

export def 'helix build' [
    --skip-compile
    --skip-pull
] {
    let dest = '/opt/helix/bin'
    let p = $dest | path parse | get parent
    let etc = $p | path join 'etc'
    sudo mkdir -p $etc
    sudo cp -f ./helix/* $etc

    cd ~/world/helix/
    if not $skip_compile {
        if not $skip_pull {
            git pull
        }
        cargo xtask steel
    }
    cp target/release/hx ~/.cargo/bin/hx
    tar cf - --exclude=runtime/grammars/sources runtime
    | sudo tar xvf - -C $dest

    cd ~/.cargo/bin
    for i in [hx steel-language-server] {
        strip -s $i
        mv -f $i $dest
    }
    #ln -fs ($dest | path join "hx") hx
    cd $p
    tar cvf - bin etc | zstd -19 -T0 | save -f ~/Downloads/helix-steel.tar.zst
}
