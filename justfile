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