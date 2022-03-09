# 常用软件
zsh
vim
git
tmux
podman
sqlite
jq tree
wget curl xh
openssh-server

# 图形界面
awesome rofi
scrot
xscreensaver
krusader
liferea

vscode
    ms-vscode-remote.vscode-remote-extensionpack
    lfs.vscode-emacs-friendly
    ms-azuretools.vscode-docker
    brofox86.theme-espresso-soda-solarized
vivaldi
freefilesync

ibus-rime
    fcitx-tools
    fcitx-table-wubi
fonts
    JetBrains Mono
    SansCode
    Hasklig
    Operator Mono Lig

gnome-tweak-tool
gnome-shell-extensions
chrome-gnome-shell


# 更改默认 shell
chsh -s /bin/zsh
# 添加用户到组
gpasswd -a user docker
#usermod -a -G docker user
chmod -R go-w *
# sudo 免密码
sed -i 's/^.*\(%sudo.*\)ALL$/\1NOPASSWD: ALL/g' /etc/sudoers
#echo '%sudo ALL=(ALL) NOPASSWD: ALL' >> /etc/sudoers

sudo passwd -l root

# Server
# sshd disable password login
sed -i 's/#\(PasswordAuthentication\).*$/\1 no/g' /etc/ssh/sshd_config
echo "Match Address 10.0.0.0/8,172.178.0.0/16,192.168.0.0/16\n    PasswordAuthentication yes" >> /etc/ssh/sshd_config
# 修改运行级别 (服务器模式)
systemctl set-default multi-user.target
