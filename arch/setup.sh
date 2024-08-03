parted -s /dev/sda -- mklabel gpt
parted -s --align=optimal /dev/sda -- mkpart ESP fat32 1MB 512MB
parted -s /dev/sda -- set 1 esp on
parted -s --align=optimal /dev/sda -- mkpart primary 512MB 100%
mkfs.fat -F 32 -n boot /dev/sda1
mkfs.btrfs -f -L arch /dev/sda2
mount /dev/sda2 /mnt
btrfs subvolume create /mnt/@
btrfs subvolume create /mnt/@home
btrfs subvolume create /mnt/@var
btrfs subvolume create /mnt/@snapshots
btrfs subvolume create /mnt/@swap
umount /mnt
mount -o compress=zstd,subvol=@ /dev/sda2 /mnt
mkdir -p /mnt/{efi,etc,home,var,.snapshots,swap}
mount /dev/sda1 /mnt/efi
mount -o compress=zstd,subvol=@home /dev/sda2 /mnt/home
mount -o compress=zstd,noatime,subvol=@var /dev/sda2 /mnt/var
mount -o compress=zstd,noatime,subvol=@snapshots /dev/sda2 /mnt/.snapshots
mount -o subvol=@swap /dev/sda2 /mnt/swap
touch /mnt/swap/swapfile
chattr +C /mnt/swap/swapfile
chmod 600 /mnt/swap/swapfile
dd if=/dev/zero of=/mnt/swap/swapfile bs=1G count=8
mkswap -L swap /mnt/swap/swapfile
swapon /mnt/swap/swapfile
sed -i '1i Server = https://mirrors.tuna.tsinghua.edu.cn/archlinux/$repo/os/$arch' /etc/pacman.d/mirrorlist
pacstrap -K /mnt base base-devel linux linux-zen linux-firmware btrfs-progs grub efibootmgr snapper grub-btrfs reflector mtools os-prober dosfstools pipewire pipewire-alsa pipewire-pulse pipewire-jack wireplumber networkmanager resolvconf dhcpcd iw wireguard-tools sudo fakeroot debugedit xdg-user-dirs curl jq openssh rsync tree net-tools htop git neovim nushell sqlite ripgrep fd dust bottom alacritty neovide gcc gdb cmake ninja clang nodejs npm python-pip rustup podman buildah skopeo kubectl helm amd-ucode bluez bluez-utils blueman tlp tlp-rdw acpi acpi_call noto-fonts noto-fonts-emoji ttf-ubuntu-font-family ttf-dejavu ttf-freefont ttf-liberation ttf-droid ttf-roboto terminus-font xdotool xclip vivaldi chromium gparted nm-connection-editor networkmanager-openvpn sddm plasma-desktop plasma-pa plasma-nm plasma-systemmonitor kscreen kvantum powerdevil kdeplasma-addons kde-gtk-config breeze-gtk dolphin okular gwenview ark mpv
genfstab -U /mnt >> '/mnt/etc/fstab'
echo '### new fstab'
cat /mnt/etc/fstab
arch-chroot /mnt /bin/bash << EOF

        ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
        hwclock --systohc
        locale-gen
        echo 'LANG=en_US.UTF-8' > /etc/locale.conf
        echo 'arch_wd' > /etc/hostname
        echo 'lock root'
        passwd -l root

        grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB
        echo 'efibootmgr --create --disk /dev/sda --part 1 --loader /EFI/arch/grubx64.efi --label "GRUB"'

        grub-mkconfig -o /boot/grub/grub.cfg

        echo '%wheel ALL=(ALL:ALL) NOPASSWD: ALL' >> /etc/sudoers
        useradd -m -s /bin/nu -G wheel,storage,power,audio,video master
        #usermod -a -G wheel,storage,power,audio,video master
        echo 'set password of master'
        echo 'asdf' | passwd master --stdin
        export XDG_CONFIG_HOME=/home/master

        reflector --country chinese --fastest 10 --threads `nproc` --save /etc/pacman.d/mirrorlist
        systemctl enable NetworkManager
        systemctl enable systemd-resolved
        systemctl enable dhcpcd
        systemctl enable sshd
        git clone --depth=3 https://github.com/fj0r/nvim-lua.git $XDG_CONFIG_HOME/nvim
        opwd=$PWD
        cd $XDG_CONFIG_HOME/nvim
        nvim --headless "+Lazy! sync" +qa
        cd $opwd
        git clone --depth=3 https://github.com/fj0r/nushell.git $XDG_CONFIG_HOME/nushell
        rustup toolchain install stable
        rustup default stable
        rustup component add rust-src clippy rustfmt rust-analyzer
        rustup target add x86_64-unknown-linux-musl wasm32-wasi wasm32-unknown-unknown
        systemctl enable bluetooth
        systemctl enable tlp
        systemctl mask systemd-rfkill.service
        systemctl mask systemd-rfkill.socket
        systemctl enable sddm

        pacman -S openssl-1.1
        su master
        cd
        mkdir paru
        paru_ver=$(curl -sSL https://api.github.com/repos/Morganamilo/paru/releases/latest | jq -r '.tag_name')
        curl -sSL https://github.com/Morganamilo/paru/releases/download/${paru_ver}/paru-${paru_ver}-x86_64.tar.zst | zstd -d | tar xf - -C paru
        paru/paru -S --noconfirm paru
        rm -rf paru
        exit


        npm install --location=global yaml-language-server vscode-langservers-extracted pyright quicktype @typespec/json-schema @typespec/compiler
        pip install --no-cache-dir --break-system-packages ipython python-json-logger structlog decorator boltons pyyaml pydantic-settings typer PyParsing pydantic pytest debugpy uvicorn fastapi aiostream aiofile httpx
        cargo install rust-script cargo-wasi cargo-watch cargo-expand cargo-eval cargo-generate cargo-prefetch cargo-feature cargo-tree

        mkinitcpio -p linux
EOF
echo 'fuser -km /mnt'
umount -R /mnt
