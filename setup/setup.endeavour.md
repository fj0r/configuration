yay -S paru
pa nushell git neovim neovide alacritty zellij
pa podman buildkit skopeo
pa dust wireguard-tools wl-clipboard openbsd-netcat resolvconf jq tree
pa vivaldi wechat freefilesync smplayer qutebrowser rofi
pa fcitx5 fcitx5-gtk fcitx5-qt fcitx5-rime rime-wubi fcitx5-configtool fcitx5-chinese-addons
pa zathura zathura-pdf-mupdf calibre blender krita

# sudo 免密码
sed -i 's/# \(%.*NOPASSWD.*\)/&\n\1/' /etc/sudoers
#echo '%wheel ALL=(ALL:ALL) NOPASSWD: ALL' >> /etc/sudoers

sudo passwd -l root

# 添加用户到组
sudo gpasswd -a master lp
#sudo usermod -a -G lp master
sudo chmod -R go-w *

# Server
# sshd disable password login
sed -i 's/#\(PasswordAuthentication\).*$/\1 no/g' /etc/ssh/sshd_config
echo "Match Address 10.0.0.0/8,172.178.0.0/16,192.168.0.0/16\n    PasswordAuthentication yes" >> /etc/ssh/sshd_config
# 修改运行级别 (服务器模式)
systemctl set-default multi-user.target


# 容器
## /etc/containers/containers.conf: -> /usr/share/containers/containers.conf
    detach_keys = ""
    multi_image_archive = true
## /etc/containers/storage.conf
    [storage]
    driver = "overlay"
    runroot = "/run/containers/storage"
    graphroot = "/home/containers/storage"
    [storage.options]
    #mount_program = "/usr/bin/fuse-overlayfs"
## /etc/containers/registries.conf
    unqualified-search-registries = ["docker.io"]
    [[registry]]
    insecure = true
    location = "registry.s"
    [[registry]]
    prefix="docker.io"
    location="docker.lizzie.fun"
    [[registry]]
    prefix="ghcr.io"
    location="ghcr.lizzie.fun"


## wireguard
sudo sudo cp ~/.ssh/wg*.conf /etc/wireguard
sudo systemctl enable --now wg-quick@wg0
sudo systemctl enable --now wg-quick@wg3
