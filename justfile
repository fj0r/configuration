setup:
  #!/bin/bash
  case $(uname -sm) in
    Darwin\ *64 )
      lns='ln -fs'
      $lns $(pwd)/vscode/{keybindings.json,locale.json,projects.json,settings.json,snippets} ${HOME}/.code-portable-data/user-data/User
    ;;
    Linux\ *64 )
      lns='ln -fsr'
      $lns $(pwd)/vscode/{keybindings.json,locale.json,projects.json,settings.json,snippets} ${HOME}/.config/Code/User
    ;;
    * )
      $lns='ln -fsr'
    ;;
  esac

  $lns $(pwd)/_zshrc ~/.zshrc
  $lns $(pwd)/ssh/config ~/.ssh/config

makeSwapFile size:
  # free -g
  sudo dd if=/dev/zero of=/root/swapfile bs=1M count=$((1024*{{size}})) # GB
  sudo chmod 0600 /root/swapfile
  sudo mkswap /root/swapfile
  sudo swapon /root/swapfile
  @echo '/root/swapfile swap swap defaults 0 0' | sudo tee -a /etc/fstab

googleHost:
  @echo ""                               | sudo tee -a /etc/hosts > /dev/null
  @echo "127.0.0.1 fonts.googleapis.com" | sudo tee -a /etc/hosts
  @echo "::1 fonts.googleapis.com"       | sudo tee -a /etc/hosts

emacs-keybindings:
  gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"

grhh:
  #!/bin/bash
  c=$(pwd)
  for i in . awesome/lain awesome/revelation .zshrc.d/histdb; do
    cd $c/$i
    git reset --hard HEAD
    #git pull
  done
  cd $c

archiveNvimCfg: archiveCfg
    #!/bin/nu
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
    tar -C $tmp -cf - nvim | zstd -19 -T0 | save $"($env.HOME)/pub/nvim-cfg.tar.zst"
    rm -rf $tmp


archiveCfg:
    #!/bin/nu
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
    ls nvim/pack/packer/*/* | where type == dir | get name | each {|x|
        cd $x
        echo $'--------------($x|str trim)--------------'
        ^git reflog expire --all --expire=now
        ^git gc --prune=now --aggressive
    }
    tar -cf - nvim nushell | zstd -19 -T0 | save $"($env.HOME)/pub/cfg.tar.zst"
    exit
    rm -rf $tmp

deploy hosts:
    #!/bin/nu
    echo "should setup: git zoxide"
    let nvim = (ls $"($env.HOME)/Downloads/nvim-linux*.tar.gz" | sort-by modified -r | get name.0)
    let nushell = (ls $"($env.HOME)/Downloads/nu-*-x86_64-unknown-linux-musl.tar.gz" | sort-by modified -r | get name.0)
    let config = $"($env.HOME)/pub/cfg.tar.zst"
    [{{hosts}}] | par-each {|host|
        cat $nvim | ^ssh $host $"zstd -d | sudo tar xf - -C /usr/local/ --strip-components=1"
        cat $nushell | ^ssh $host $"zstd -d | sudo tar xf - -C /usr/local/bin/ --exclude='LICENSE' --exclude='README.txt'"
        cat $config | ^ssh $host $"bash -c 'rm -rf ~/.config/{nushell,nvim}; zstd -d | tar xf - -C ~/.config; sudo chown $\(id -u):$\(id -g) -R ~/.config/{nushell,nvim}'"
    }

