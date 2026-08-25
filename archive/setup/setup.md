- awesome(lua)
    - wezterm(lua)
        - nushell
        - nvim(lua)
        - zellij
    - rofi
    - scrot
    - vivaldi
    - ibus-rime
    - setxkbmap
    - ~~xcape~~
    - xprop

# 常用软件
git podman buildah skopeo
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
    [[registry]]
    insecure = true
    location = "registry.s"
```

openssh-server

alacritty

vivaldi qutebrowser
freefilesync [zathura(pdf) liferea]

xrandr scrot xscreensaver

`setxkbmap -option 'ctrl:swapcaps'`

ibus-rime

fonts
  - JetBrains Mono
  - SansCode
  - Hasklig
  - Operator Mono Lig


```bash
# 更改默认 shell
chsh -s /bin/zsh
# 添加用户到组
gpasswd -a user docker
#usermod -a -G docker user
chmod -R go-w *
# sudo 免密码
sed -i 's/^.*\(%sudo.*\)ALL$/\1NOPASSWD: ALL/g' /etc/sudoers
#echo '%sudo ALL=(ALL:ALL) NOPASSWD: ALL' >> /etc/sudoers

sudo passwd -l root

# Server
# sshd disable password login
sed -i 's/#\(PasswordAuthentication\).*$/\1 no/g' /etc/ssh/sshd_config
echo "Match Address 10.0.0.0/8,172.178.0.0/16,192.168.0.0/16\n    PasswordAuthentication yes" >> /etc/ssh/sshd_config
# 修改运行级别 (服务器模式)
systemctl set-default multi-user.target
```
