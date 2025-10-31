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