pacman -Sy yay

### 中文输入法
pacman -S fcitx5 fcitx5-rime fcitx5-material-color
cat <<- EOF > ~/.config/fcitx5/conf
Vertical Candidate List=False

# 按屏幕 DPI 使用
PerScreenDPI=False

# Font (设置成你喜欢的字体)
Font="Noto Sans Regular 14"

Theme=Material-Color-Black
EOF

yay -S deepin-wine-tim
yay -S tmux podman buildah skopeo
yay -S krusader akregator
yay -S blender freefilesync-bin vivaldi visual-studio-code-bin

### podman
sudo usermod -a -G podman agent
sudo touch /etc/subuid
sudo touch /etc/subgid
sudo usermod --add-subuids 100000-150000 --add-subgids 100000-150000 agent
podman system migrate

### optimus manager
yay -S optimus-manager
cat <<- EOF > /etc/optimus-manager/optimus-manager.conf
[amd]
DRI=3
driver=modesetting
tearfree=

[intel]
DRI=3
accel=
driver=modesetting
modeset=yes
tearfree=

[nvidia]
DPI=96
PAT=yes
allow_external_gpus=no
dynamic_power_management=no
ignore_abi=no
modeset=yes
options=overclocking

[optimus]
auto_logout=yes
pci_power_control=no
pci_remove=no
pci_reset=no
startup_auto_battery_mode=integrated
startup_auto_extpower_mode=nvidia
startup_mode=nvidia
switching=none
EOF


shortcut:
    - KWin
        - 窗口到桌面1,4 -> Meta+F1,F4
        - 垂直最大化窗口 -> Alt+F2
        - 最大化窗口 -> Alt+F3
        - 切换到桌面1,4 -> F1,F4
        - 显示桌面网格 -> Meta+Esc
        - 切换显示窗口(对当前桌面) -> Meta+Tab
        - 切换显示窗口(对全部桌面) -> Meta+`
    - Yakuake
        - 打开/缩回 -> Alt+`
    - 应用程序面板(小部件) -> Alt+F1 <Meta>
