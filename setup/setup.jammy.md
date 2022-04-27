# 常用软件
```bash
sudo apt install \
    zsh git tmux \
    podman buildah skopeo \
    openssh-server wireguard-tools resolvconf \
    jq tree wget curl xh sqlite
```
## 驱动
```bash
sudo ubuntu-drivers autoinstall
```

## 容器
```
/etc/containers/containers.conf: -> /usr/share/containers/containers.conf
    detach_keys = ""
    multi_image_archive = true
/etc/containers/storage.conf
    [storage]
    driver = "overlay"
    runroot = "/run/containers/storage"
    graphroot = "/home/containers/storage"
    [storage.options]
    #mount_program = "/usr/bin/fuse-overlayfs"
/etc/containers/registries.conf
    unqualified-search-registries = ["docker.io"]
    [[registry]]
    insecure = true
    location = "registry.s"
```

## wireguard
```bash
sudo cp ~s/wg*.conf /etc/wireguard

sudo systemctl enable --now wg-quick@wg0
sudo systemctl enable --now wg-quick@wg3
```

## ime
```bash
sudo apt install ibus-rime
cd ~/data/rime-wubi
sudo cp -f wubi86_fg* pinyin_simp* /usr/share/rime-data/
sudo yq -i e '.schema_list[0].schema="wubi86_fg_pinyin"' /usr/share/rime-data/default.yaml
```

## fonts
  - JetBrains Mono
  - SansCode
  - Hasklig
  - Operator Mono Lig

```bash
sudo cp -r ~plt/fonts/JetBrainsMono /usr/share/fonts
sudo fc-cache -fv
```

## clash
```bash
cd ~/data/clash
just run
sudo cp ~c/systemd/clash.service /etc/systemd/system
sudo systemctl daemon-reload
sudo systemctl enable --now clash.service
```

## gitea
```bash
cd ~/data/gitea
bash start.sh
sudo cp ~c/systemd/gitea* /etc/systemd/system
sudo systemctl daemon-reload
sudo systemctl enable --now gitea
```

## qutebrowser
```bash
sudo apt install rofi keyutils
sudo npm install -g @bitwarden/cli
```

# GUI APP
- liferea
- alacritty
- calibre

awesome rofi
zathura(pdf)

xrandr scrot xscreensaver

vivaldi freefilesync

`setxkbmap -option 'ctrl:swapcaps'`



```bash
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
```
