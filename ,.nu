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

export module helix {
    def cmpl-pr [] {
        const data = path self helix/data.yaml
        open $data
        | get subscription
        | each {|x|
            let value = $x.url | split row '/' | last
            { value: $value, description: $x.title }
        }
        | { completions: $in, options: { sort: false } }
    }
    export def merge [pr:int@cmpl-pr] {
        cd ~/world/helix/
        gh pr checkout $pr
    }

    export def build [
        --skip-compile
        --skip-pull
        --skip-steel
    ] {
        let dest = '/opt/helix/bin'
        let p = $dest | path parse | get parent
        sudo rm -rf $p
        sudo mkdir -p $dest

        let config = $p | path join 'config'
        sudo mkdir -p $config
        sudo cp -rf ./helix/* $config

        cd ~/world/helix/
        cp -r ~/world/moonbit.helix/runtime/queries/moonbit/ runtime/queries/
        if not $skip_compile {
            if not $skip_pull {
                git pull
            }
            # $env.RUSTFLAGS = "-C target-feature=-crt-static"
            if not $skip_steel {
                cargo xtask steel
            }
        }
        sudo cp target/release/hx $dest
        tar cf - --exclude=runtime/grammars/sources runtime
        | sudo tar xvf - -C $dest

        cd ~/.cargo/bin
        mut ms = []
        if not $skip_steel {
            $ms ++= [steel-language-server]
        }
        for i in $ms {
            strip -s $i
            sudo cp -f $i $dest
        }
        sudo cp /usr/bin/yazi $dest
        #ln -fs ($dest | path join "hx") hx
        cd $p
        tar cvf - bin config | zstd -19 -T0 | save -f ~/Downloads/helix-steel.tar.zst
    }

    export def push [image: string = "ghcr.io/fj0r/data:helix"] {
        oci wrap $image --author "fj0r" {|d|
            cat ~/Downloads/helix-steel.tar.zst
            | zstd -d
            | tar xvf - -C $d
        }
    }
}

export def xtools [image: string = "ghcr.io/fj0r/data:xtools"] {

}
