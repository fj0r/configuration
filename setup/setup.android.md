- termux
- readera
- vivaldi
- kdeconnect
- mattermost
- bitwarden

### termux
##### proot-distro
```
pkg install proot-distro
proot-distro install ubuntu
#echo 'exec proot-distro login ubuntu' >> ~/.profile
```
##### utils
```
apt install neovim zsh git tmux
chsh -s zsh
apt install python nodejs build-essential
apt install ripgrep
```

##### ssh
```
apt install openssh
eco 'sshd' >> .profile
```
or
```
apt install dropbear
echo 'dropbear -Rms -p 2222' >> .profile
```
##### k8s client
```
apt install kubectl
```

##### font
```
scp ~plt/fonts/JetBrainsMono/JetBrainsMono-ExtraLight.ttf mi:~/.termux/font.ttf
```

##### extra keys
`Volume Up + q`

##### code server
```
proot-distro login ubuntu
curl -fsSL https://code-server.dev/install.sh | sh
code-server
```
##### storage
```
termux-setup-storage
```

##### setting
~/.termux/termux.properties
