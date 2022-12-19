sudo apt install curl jq zstd podman git wireguard-tools libfuse2 xclip xsel resolvconf
curl ${layer_host}/utils.tar.zst | zstd -d | sudo tar -xf - --strip-components=1 -C /usr/local/bin/
curl ${layer_host}/nushell.tar.zst | zstd -d | sudo tar -xf - --strip-components=1 -C /usr/local/bin/
curl ${layer_host}/nvim.tar.zst | zstd -d | sudo tar -xf - --strip-components=1 -C /usr/local/

sudo sed -i 's!#*\s*\(unqualified-search-registries\) = .*!\1 = ["docker.io"]!' /etc/containers/registries.conf

sudo ln -fs /usr/share/containers/containers.conf /etc/containers/containers.conf

sudo sed -i 's!#*\s*\(multi_image_archive\) = .*!\1 = true!' /etc/containers/containers.conf

# clash
cd ~/data/clash
just run

# wezterm
export wez_version=$(curl -s https://api.github.com/repos/wez/wezterm/releases/latest | jq -r '.tag_name')
wget https://github.com/wez/wezterm/releases/download/$wez_version/wezterm-$wez_version.Ubuntu22.04.deb
sudo dpkg -i ./wezterm-$wez_version.Ubuntu22.04.deb

# curl -sSL https://github.com/wez/wezterm/releases/download/${wez_version}/WezTerm-${wez_version}-Ubuntu18.04.AppImage -o ~/Downloads/wezterm
# chmod +x ~/Downloads/wezterm
# sudo cp ~/Downloads/wezterm /usr/local/bin

# vivaldi
# https://vivaldi.com/zh-hans/download/
curl -sSL https://repo.vivaldi.com/stable/linux_signing_key.pub | gpg --import -
sudo sh -c 'echo "deb http://repo.vivaldi.com/stable/deb/ stable main" >> /etc/apt/sources.list.d/vivaldi.list'
sudo apt update
sudo apt install vivaldi-stable


sudo apt install gnome-shell-extensions
# https://extensions.gnome.org/extension/545/hide-top-bar/
#

# kubectl
sudo podman run --rm -v /usr/local/bin:/x 0x:k8s cp /usr/local/bin/kubectl /x

# fcitx
sudo apt install fcitx5 fcitx5-rime fcitx5-material-color rime-data-wubi

# freefilesync

# other
sudo apt install \
    vlc \
    calibre
