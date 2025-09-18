let packages = [
    [paru]
    [git nushell duckdb neovim neovide helix alacritty zellij]
    [podman buildah skopeo kubectl kubeadm helm]
    [rustup sccache cmake nodejs npm uv]
    [wireguard-tools wl-clipboard openbsd-netcat resolvconf]
    [jq tree dust glow termshark]
    [vivaldi zed freefilesync smplayer qutebrowser flameshot wps-office rofi]
    [wechat telegram-desktop]
    [fcitx5 fcitx5-gtk fcitx5-qt fcitx5-rime rime-wubi fcitx5-configtool fcitx5-chinese-addons]
    [blender krita calibre zathura zathura-pdf-mupdf]
    [hyprland quickshell]  # qt5compat qtsvg qtimageformats qtmultimedia
]

def install-pkgs [] {
    yay -S ...($packages | flatten)
}

def setup-nopassword [] {
    sudo sed -i 's/# \(%.*NOPASSWD.*\)/&\n\1/' /etc/sudoers
    sudo rm /etc/sudoers.d/10-installer
    #sudo passwd -l root
}
