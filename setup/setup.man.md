## 基础
### tools
```bash
pacman -Sy yay curl xh ripgrep jq go-yq dust bottom
```

### utilities
```bash
yay -S tmux zoxide xclip

export nvim_url=$(curl -sSL https://api.github.com/repos/neovim/neovim/releases -H 'Accept: application/vnd.github.v3+json' \
    | jq -r '.[0].assets[].browser_download_url' | grep -v sha256sum | grep linux64.tar.gz)
curl -sSL ${nvim_url} | tar zxf - -C /usr/local --strip-components=1

nu_url=$(curl -sSL https://api.github.com/repos/nushell/nushell/releases -H 'Accept: application/vnd.github.v3+json' \
    | jq -r '[.[]|select(.prerelease == false)][0].assets[].browser_download_url' | grep x86_64-unknown-linux-musl)
curl -sSL ${nu_url} | tar zxf - -C /usr/local/bin --wildcards 'nu*'
```

### 字体
```bash
yay -S ttf-jetbrains-mono adobe-source-han-sans-cn-fonts adobe-source-han-serif-cn-fonts
```

## 容器
```bash
yay -S podman buildah skopeo

sudo usermod -a -G podman agent
sudo touch /etc/subuid
sudo touch /etc/subgid
sudo usermod --add-subuids 100000-150000 --add-subgids 100000-150000 agent
podman system migrate
```
## 中文输入法
```bash
yay -S fcitx5 fcitx5-gtk fcitx5-qt fcitx5-rime fcitx5-material-color rime-wubi

cat <<- EOF > ~/.config/fcitx5/conf/ui.conf
Vertical Candidate List=False

# 按屏幕 DPI 使用
PerScreenDPI=False

# Font (设置成你喜欢的字体)
Font="Source Han Serif Regular 14"

Theme=Material-Color-Black
EOF
```

### 自定义码表
```
cd ~/data/rime-wubi
sudo cp -f wubi86_fg* pinyin_simp* /usr/share/rime-data/
sudo yq -i e '.schema_list[0].schema="wubi86_fg_pinyin"' /usr/share/rime-data/default.yaml
# re-deploy rime
```

## ext
```bash
yay -S wireguard-tools
yay -S vivaldi visual-studio-code-bin
yay -S akregator krusader
yay -S blender freefilesync-bin
yay -S deepin-wine-tim
```

### qutebrowser
```bash
yay -S qutebrowser rofi python-pip nodejs npm
sudo pip install tldextract
sudo npm install -g @bitwarden/cli
```

## 桌面环境

### krohnkite
[download](https://github.com/esjeon/krohnkite/releases/download/v0.8.1/krohnkite-0.8.1.kwinscript)


### shortcut:
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

## optimus manager (unnecessary)
```bash
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
```
