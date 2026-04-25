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
        --with-steel
        --musl
    ] {
        const self = path self .
        let dest = '/opt/helix/bin'

        cd ~/world/helix/
        # cp -r ~/world/moonbit.helix/runtime/queries/moonbit/ runtime/queries/
        if not $skip_compile {
            if not $skip_pull {
                git pull
            }
            if $musl {
                $env.RUSTFLAGS = "-C target-feature=-crt-static -C link-arg=-static -l m"
                $env.CC = '/bin/musl-gcc'
                $env.CXX = '/bin/musl-g++'
            }
            if $with_steel {
                cargo xtask steel
            } else {
                mut args = [--path helix-term --locked --features 'steel,git' --force]
                if $musl {
                    $args ++= [--target x86_64-unknown-linux-musl]
                }
                print $"::::(ansi grey)cargo install ($args)(ansi reset)"
                cargo install ...$args
            }
        }

        let p = $dest | path parse | get parent
        sudo rm -rf $p
        sudo mkdir -p $dest

        sudo cp target/release/hx $dest
        tar cf - --exclude=runtime/grammars/sources runtime
        | sudo tar xvf - -C $dest

        let config = $p | path join 'etc' 'helix'
        sudo mkdir -p $config
        cd $self
        sudo cp -rf ./helix/* $config

        cd ~/.cargo/bin
        mut ms = []
        if $with_steel {
            $ms ++= [steel-language-server]
        }
        for i in $ms {
            strip -s $i
            sudo cp -f $i $dest
        }
        sudo cp /usr/bin/yazi $dest

        #ln -fs ($dest | path join "hx") hx
        cd $p
        sudo mkdir -p share/steel/cogs
        tar -cf - --exclude=target -C ~/.local/share/steel/cogs . | sudo tar -xf - -C share/steel/cogs
        tar -cvf - --transform='s#^./##' . | zstd -19 -T0 | save -f ~/Downloads/helix-steel.tar.zst
    }

    export def push [image: string = "ghcr.io/fj0r/assets:helix"] {
        use docker *
        use lg *
        oci wrap $image --author "fj0r" {|d|
            cat ~/Downloads/helix-steel.tar.zst
            | zstd -d
            | tar xvf - -C $d
        }
    }

    export def unshare-push [] {
        const s = path self
        buildah unshare nu -c $"use lg; use docker *; overlay use ($s); helix push"
    }
}

export def xtools [image: string = "ghcr.io/fj0r/data:xtools"] {

}
