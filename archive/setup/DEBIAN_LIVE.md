# Prerequisites
```sh
sudo apt-get install \
    debootstrap \
    squashfs-tools \
    xorriso \
    grub-pc-bin \
    grub-efi-amd64-bin \
    mtools
```

# Bootstrap and Configure Debian
```sh
sudo debootstrap \
    --arch=i386 \
    --variant=minbase \
    buster \
    $HOME/LIVE_BOOT/chroot \
    https://mirror.tuna.tsinghua.edu.cn/debian
```

# chroot
```sh
sudo chroot $HOME/LIVE_BOOT/chroot
```

```sh
echo "debian-live" > /etc/hostname

apt-cache search linux-image

apt-get update && \
apt-get install --no-install-recommends \
    linux-image-686 \
    live-boot \
    systemd-sysv

apt-get install apt-transport-https gpg sudo software-properties-common curl wget

wget -qO- https://repo.vivaldi.com/archive/linux_signing_key.pub | sudo apt-key add -
add-apt-repository 'deb https://repo.vivaldi.com/archive/deb/ stable main'

curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
install -o root -g root -m 644 packages.microsoft.gpg /usr/share/keyrings/
sh -c 'echo "deb [arch=amd64 signed-by=/usr/share/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/vscode stable main" > /etc/apt/sources.list.d/vscode.list'

curl -sL https://deb.nodesource.com/setup_12.x | bash -

apt-get update
apt-get install --no-install-recommends \
    network-manager net-tools wireless-tools wpagui \
    wget curl openssh-client openssh-server \
    awesome xserver-xorg-core xserver-xorg xinit xterm \
    rofi tilix scrot ibus-table-wubi \
    zsh vim git tmux rsync sqlite3 \
    jq tree httpie build-essential \
    nodejs python3 docker.io\
    xfsprogs btrfs-progs \
     && \
apt-get install vivaldi-stable code \
apt-get clean

passwd root
chsh -s /bin/zsh
usermod -a -G docker root
sed -i 's/^.*\(%sudo.*\)ALL$/\1NOPASSWD: ALL/g' /etc/sudoers
sed -i 's/#\(PasswordAuthentication\).*$/\1 no/g' /etc/ssh/sshd_config

exit
```

# squashfs
```sh
sudo mkdir -p $HOME/LIVE_BOOT/{scratch,image/live}
sudo mksquashfs \
    $HOME/LIVE_BOOT/chroot \
    $HOME/LIVE_BOOT/image/live/filesystem.squashfs \
    -e boot
sudo cp $HOME/LIVE_BOOT/chroot/boot/vmlinuz-* \
    $HOME/LIVE_BOOT/image/vmlinuz && \
sudo cp $HOME/LIVE_BOOT/chroot/boot/initrd.img-* \
    $HOME/LIVE_BOOT/image/initrd
cat <<'EOF' >$HOME/LIVE_BOOT/scratch/grub.cfg

search --set=root --file /DEBIAN_CUSTOM

insmod all_video

set default="0"
set timeout=30

menuentry "Debian Live" {
    linux /vmlinuz boot=live quiet nomodeset
    initrd /initrd
}
EOF
sudo touch $HOME/LIVE_BOOT/image/DEBIAN_CUSTOM

```

# Create Bootable USB
```sh
export disk=/dev/sda

sudo mkdir -p /mnt/{usb,efi}

sudo parted --script $disk \
    mklabel gpt \
    mkpart primary fat32 2048s 4095s \
        name 1 BIOS \
        set 1 bios_grub on \
    mkpart ESP fat32 4096s 413695s \
        name 2 EFI \
        set 2 esp on \
    mkpart primary fat32 413696s 100% \
        name 3 LINUX \
        set 3 msftdata on

sudo gdisk $disk << EOF
r     # recovery and transformation options
h     # make hybrid MBR
1 2 3 # partition numbers for hybrid MBR
N     # do not place EFI GPT (0xEE) partition first in MBR
EF    # MBR hex code
N     # do not set bootable flag
EF    # MBR hex code
N     # do not set bootable flag
83    # MBR hex code
Y     # set the bootable flag
x     # extra functionality menu
h     # recompute CHS values in protective/hybrid MBR
w     # write table to disk and exit
Y     # confirm changes
EOF

sudo mkfs.vfat -F32 ${disk}2 && \
sudo mkfs.vfat -F32 ${disk}3

sudo mount ${disk}2 /mnt/efi && \
sudo mount ${disk}3 /mnt/usb

sudo grub-install \
    --target=x86_64-efi \
    --efi-directory=/mnt/efi \
    --boot-directory=/mnt/usb/boot \
    --removable \
    --recheck

sudo grub-install \
    --target=i386-pc \
    --boot-directory=/mnt/usb/boot \
    --recheck \
    $disk

sudo mkdir -p /mnt/usb/{boot/grub,live}

sudo cp -r $HOME/LIVE_BOOT/image/* /mnt/usb/

sudo cp \
    $HOME/LIVE_BOOT/scratch/grub.cfg \
    /mnt/usb/boot/grub/grub.cfg

sudo umount /mnt/{usb,efi}

```