export def "x archive cfg nvim" [] {
    let d = (date now | date format "%Y%m%d%H%M%S")
    let tmp = $"/tmp/cfg/($d)"
    let curr = $env.PWD
    mkdir $tmp
    (tar
            --exclude='*/__pycache__*'
            --exclude='*/nvim-luapad/gifs*'
            --exclude='*/plugged/ultisnips/doc/*'
            --exclude='*/plugged/vimspector/gadgets/*'
            --exclude='plugged/LeaderF/autoload/leaderf/fuzzyMatch_C/build'
            --exclude='pack/packer/*/*/.github'
            --exclude='pack/packer/*/*/.git'
            --exclude='.git*'
            -cf - nvim | tar -xf - -C $tmp
    )
    rm -f $"($tmp)/nvim/.netrwhist"
    enter $tmp
    tar -zcf cfg.tar.gz nvim
    rm -f $"($env.HOME)/pub/nvim-cfg.tar.gz"
    mv cfg.tar.gz $"($env.HOME)/pub/nvim-cfg.tar.gz"
    exit
    rm -rf $tmp
}

export def "x archive cfg" [] {
    let d = (date now | date format "%Y%m%d%H%M%S")
    let tmp = $"/tmp/cfg/($d)"
    let curr = $env.PWD
    mkdir $tmp
    (tar
            --exclude='*/__pycache__*'
            --exclude='*/nvim-luapad/gifs*'
            --exclude='*/plugged/ultisnips/doc/*'
            --exclude='*/plugged/vimspector/gadgets/*'
            --exclude='plugged/LeaderF/autoload/leaderf/fuzzyMatch_C/build'
            --exclude='pack/packer/*/*/.github'
            -cf - nvim | tar -xf - -C $tmp
    )
    rm -f $"($tmp)/nvim/.netrwhist"
    enter $tmp
    cp -r $"($curr)/nushell" .
    cp -r $"($curr)/zellij" .
    ls nvim/pack/packer/*/* | where type == dir | get name | each {|x|
        cd $x
        echo $'--------------($x|str trim)--------------'
        ^git reflog expire --all --expire=now
        ^git gc --prune=now --aggressive
    }
    tar -zcf cfg.tar.gz nvim nushell zellij 
    rm -f $"($env.HOME)/pub/cfg.tar.gz"
    mv cfg.tar.gz $"($env.HOME)/pub/cfg.tar.gz"
    exit
    rm -rf $tmp
}

def "nu-complete ssh host" [] {
    rg -LNI '^Host [a-z0-9_\-\.]+' ~/.ssh | lines | each {|x| $x | split row ' '| get 1}
}

export def "x deploy via ssh" [...hosts: string@"nu-complete ssh host"] {
    echo "should setup: zellij git zoxide"
    let nvim = (ls $"($env.HOME)/Downloads/nvim-linux*.tar.gz" | sort-by modified -r | get name.0)
    let nushell = (ls $"($env.HOME)/Downloads/nu-*-x86_64-unknown-linux-musl.tar.gz" | sort-by modified -r | get name.0)
    let config = $"($env.HOME)/pub/cfg.tar.gz"
    $hosts | par-each {|host|
        cat $nvim | ^ssh $host $"sudo tar zxf - -C /usr/local/ --strip-components=1"
        cat $nushell | ^ssh $host $"sudo tar zxf - -C /usr/local/bin/ --exclude='LICENSE' --exclude='README.txt'"
        cat $config | ^ssh $host $"bash -c 'rm -rf ~/.config/{nushell,nvim,zellij}; tar zxf - -C ~/.config; sudo chown $\(id -u):$\(id -g) -R ~/.config/{nushell,nvim,zellij}'"
    }
}
