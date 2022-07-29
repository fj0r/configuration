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
    cp -r $"($curr)/tmux" .
    ls nvim/pack/packer/*/* | where type == dir | get name | each {|x|
        cd $x
        echo $'--------------(pwd|str trim)--------------'
        ^git reflog expire --all --expire=now
        ^git gc --prune=now --aggressive
    }
    tar -zcf cfg.tar.gz nvim nushell tmux
    rm -f $"($env.HOME)/pub/cfg.tar.gz"
    mv cfg.tar.gz $"($env.HOME)/pub/cfg.tar.gz"
    exit
    rm -rf $tmp
}
