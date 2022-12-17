sudo apt install curl jq zstd podman git wireguard-tools libfuse2 xclip xsel

curl ${layer_host}/utils.tar.zst | zstd -d | sudo tar -xf - --strip-components=1 -C /usr/local/bin/
curl ${layer_host}/nushell.tar.zst | zstd -d | sudo tar -xf - --strip-components=1 -C /usr/local/bin/
curl ${layer_host}/nvim.tar.zst | zstd -d | sudo tar -xf - --strip-components=1 -C /usr/local/

sudo nvim /etc/containers/registries.conf
# unqualified-search-registries = ["docker.io"]

sudo ln -fs /usr/share/containers/containers.conf /etc/containers/containers.conf

sudo sed -i 's!#*\(multi_image_archive\) = .*!\1 = true!' /etc/containers/containers.conf

# clash
cd ~/data/clash
just run

# wezterm
curl -sSL https://github.com/wez/wezterm/releases/download/20221119-145034-49b9839f/WezTerm-20221119-145034-49b9839f-Ubuntu18.04.AppImage -o ~/Downloads/wezterm
chmod +x ~/Downloads/wezterm
sudo cp ~/Downloads/wezterm /usr/local/bin

# vivaldi
# https://vivaldi.com/zh-hans/download/


sudo apt install gnome-shell-extensions
# https://extensions.gnome.org/extension/545/hide-top-bar/
#

# kubectl

# fcitx
