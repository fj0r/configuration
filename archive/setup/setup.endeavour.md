# packages
yay -S paru
yay -S git nushell duckdb neovim neovide helix alacritty zellij
yay -S podman buildah skopeo kubectl kubeadm helm
yay -S rustup sccache cmake nodejs npm uv
yay -S wireguard-tools wl-clipboard openbsd-netcat resolvconf
yay -S jq tree dust glow termshark
yay -S vivaldi zed freefilesync smplayer qutebrowser flameshot wps-office rofi
yay -S wechat telegram-desktop
yay -S fcitx5 fcitx5-gtk fcitx5-qt fcitx5-rime rime-wubi fcitx5-configtool fcitx5-chinese-addons
yay -S blender krita calibre zathura zathura-pdf-mupdf ttf-lilex
yay -S hyprland quickshell # qt5compat qtsvg qtimageformats qtmultimedia

# sudo 免密码
sed -i 's/# \(%.*NOPASSWD.*\)/&\n\1/' /etc/sudoers
#echo '%wheel ALL=(ALL:ALL) NOPASSWD: ALL' >> /etc/sudoers

rm /etc/sudoers.d/10-installer

sudo passwd -l root

# 添加用户到组
sudo gpasswd -a master lp
#sudo usermod -a -G lp master
sudo chmod -R go-w *

## wireshark
sudo groupadd wireshark
sudo usermod -aG wireshark master
sudo setcap 'CAP_NET_RAW+eip CAP_NET_ADMIN+eip' (whereis dumpcap | split row -r '\s+' | get 1)

# sshd disable password login
sed -i 's/#\(PasswordAuthentication\).*$/\1 no/g' /etc/ssh/sshd_config
echo "Match Address 10.0.0.0/8,172.178.0.0/16,192.168.0.0/16\n    PasswordAuthentication yes" >> /etc/ssh/sshd_config

# 修改运行级别 (服务器模式)
systemctl set-default multi-user.target

# swapfile
sudo dd if=/dev/zero of=/swapfile bs=1G count=32
sudo chmod 600 /swapfile
sudo mkswap -L swap /swapfile
#sudo chattr +C /swapfile
sudo swapon /swapfile
echo '/swapfile swap swap defaults 0 0' | sudo tee -a /etc/fstab
sudo swapon --show
sudo sysctl vm.swappiness=10

# sysctl
cat <<- EOF > /etc/sysctl.d/99-master.conf
## 减少交换使用
vm.swappiness=10

## 文件系统缓存压力
vm.vfs_cache_pressure = 50

## 脏页回写设置
vm.dirty_ratio = 30
vm.dirty_background_ratio = 10

## 增加文件描述符限制
fs.file-max = 1000000

## 增加inode数量
fs.inotify.max_user_watches = 524288

## 启用 IP 转发
net.ipv4.ip_forward = 1
net.ipv6.conf.all.forwarding = 1

## 启用 BBR 拥塞控制算法
net.ipv4.tcp_congestion_control = bbr
net.core.default_qdisc = fq

## 增加网络缓冲区大小
net.core.rmem_max = 16777216
net.core.wmem_max = 16777216
net.ipv4.tcp_rmem = 4096 87380 16777216
net.ipv4.tcp_wmem = 4096 65536 16777216

EOF

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


# wireguard
sudo sudo cp ~/.ssh/wg*.conf /etc/wireguard
sudo systemctl enable --now wg-quick@wg0
sudo systemctl enable --now wg-quick@wg3

# disable file search
balooctl6 suspend
balooctl6 disable
balooctl6 purge

# /etc/sddm.conf.d/dpi.conf
    [Wayland]
    EnableHiDPI=true

    [General]
    GreeterEnvironment=QT_SCREEN_SCALE_FACTORS=2,QT_FONT_DPI=192

# /etc/default/grub
    GRUB_GFXMODE=1280x960,auto
    GRUB_GFXPAYLOAD_LINUX=keep
    #GRUB_BACKGROUND='/usr/share/endeavouros/splash.png'
