export def main [] {
    let cfg = $in
    print ($cfg | table -e)

    for i in (partitions -cf) {
        print $"($i)"
    }

    for i in (components -kcdv --lang [c js py rs hs] | setup -h 'arch_wd' -p asdf) {
        print $i
    }
}


def setup [
    --mnt: string = /mnt
    --master (-m): string = master
    --password (-p): string
    --hostname (-h): string
    --disk (-d): string = '/dev/sda'
] {
    let components = $in | group-by type
    let nl = char newline
    let tab = char tab
    mut cmds = []
    $cmds ++= $"sed -i '1i Server = https://mirrors.tuna.tsinghua.edu.cn/archlinux/$repo/os/$arch' /etc/pacman.d/mirrorlist"
    mut sys_pkg = $components.sys.name
    $cmds ++= $"pacstrap -K ($mnt) ($sys_pkg | str join ' ')"
    $cmds ++= $"genfstab -U ($mnt) >> '($mnt)/etc/fstab'"
    $cmds ++= [
        $"echo '### new fstab'"
        $"cat ($mnt)/etc/fstab"
    ]

    let paru = $"
        pacman -S openssl-1.1
        su ($master)
        cd
        mkdir paru
        paru_ver=$\(curl -sSL https://api.github.com/repos/Morganamilo/paru/releases/latest | jq -r '.tag_name'\)
        curl -sSL https://github.com/Morganamilo/paru/releases/download/${paru_ver}/paru-${paru_ver}-($nu.os-info.arch).tar.zst | zstd -d | tar xf - -C paru
        paru/paru -S --noconfirm paru
        rm -rf paru
        exit"
    let pkgs = $components | items {|k, v|
        let idt = '        '
        match $k {
            pip => {
                $"($idt)pip install --no-cache-dir --break-system-packages ($v.name | str join ' ')"
            }
            npm => {
                $"($idt)npm install --location=global ($v.name | str join ' ')"
            }
            cargo => {
                $"($idt)cargo install ($v.name | str join ' ')"
            }
            aur => {
                $"($idt)paru -S --noconfirm ($v.name | str join ' ')"
            }
            _ => ''
        }
    }
    | str join $nl
    let post = $components.sys
    | each {|x| if ($x.post? | is-empty) {[]} else {$x.post} }
    | flatten
    | str join $'($nl)        '
    let chroot_cmd = $"
        ln -sf /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
        hwclock --systohc
        locale-gen
        echo 'LANG=en_US.UTF-8' > /etc/locale.conf
        echo '($hostname)' > /etc/hostname
        echo 'lock root'
        passwd -l root

        grub-install --target=x86_64-efi --efi-directory=/efi --bootloader-id=GRUB
        echo 'efibootmgr --create --disk ($disk) --part 1 --loader /EFI/arch/grubx64.efi --label \"GRUB\"'

        grub-mkconfig -o /boot/grub/grub.cfg

        echo '%wheel ALL=\(ALL:ALL\) NOPASSWD: ALL' >> /etc/sudoers
        useradd -m -s /bin/nu -G wheel,storage,power,audio,video ($master)
        #usermod -a -G wheel,storage,power,audio,video ($master)
        echo 'set password of ($master)'
        echo '($password)' | passwd ($master) --stdin
        export XDG_CONFIG_HOME=/home/($master)

        ($post)"


    $cmds ++= ([
        $"arch-chroot ($mnt) /bin/bash << EOF"
        $chroot_cmd
        $paru
        $pkgs
        "
        mkinitcpio -p linux"
        "EOF"
        $"echo 'fuser -km ($mnt)'"
        $"umount -R ($mnt)"
    ] | str join $nl)
    $cmds
}

def components [
    --kde (-k)
    --container (-c)
    --dev (-d)
    --vpn (-v)
    --lang (-l): list<string> = []
] {
    let manifest = open pacman.yml
    mut r = [core network sys base hardware ...$lang]
    if $container {
        $r ++= [container kubernetes]
    }
    if $vpn {
        $r ++= [vpn]
    }
    if $kde {
        $r ++= [audio font x kde]
    }
    if $dev {
        $r ++= [dev]
    }
    let r = $r | uniq
    $manifest
    | filter {|x|
        $x.tags | all {|y| $y in $r }
    }
    | each {|x|
        if ($x.type? | is-empty) {
            { ...$x, type: sys }
        } else {
            $x
        }
    }
}


def partitions [
    disk='/dev/sda'
    mnt='/mnt'
    boot='/mnt/efi'
    --label (-l) = 'arch'
    --swapsize=8
    --with-format (-f)
    --with-create (-c)
] {
    let dev_boot = $"($disk)1"
    let dev = $"($disk)2"
    let sv = [[vol dir                  mnt ];
        [home      home                 [zstd]]
        [var       var                  [zstd noatime]]
        [snapshots .snapshots           [zstd noatime]]
        [swap      swap                 []]
    ]
    let ms = {
        root: [
            $"mount -o compress=zstd,subvol=@ ($dev) ($mnt)"
        ]
        boot: [
            $"mount ($dev_boot) ($boot)"
        ]
        sub: ($sv | each {|x|
            let opt = $x.mnt
            | each {|y|
                match $y {
                    zstd => 'compress=zstd'
                    _ => $y
                }
            }
            | append $"subvol=@($x.vol)"
            | str join ','
            $"mount -o ($opt) ($dev) ($mnt)/($x.dir)"
        })
        swap: [
            $"swapon ($mnt)/swap/swapfile"
        ]
    }
    let create_cmd = if $with_create {
        [
            $"parted -s ($disk) -- mklabel gpt"
            $"parted -s --align=optimal ($disk) -- mkpart ESP fat32 1MB 512MB"
            $"parted -s ($disk) -- set 1 esp on"
            $"parted -s --align=optimal ($disk) -- mkpart primary 512MB 100%"
            $"mkfs.fat -F 32 -n boot ($dev_boot)"
            $"mkfs.btrfs -f -L ($label) ($dev)"
        ]
    } else {
        []
    }
    let mount_cmd = if $with_format {
        [
            $"mount ($dev) ($mnt)"
            $"btrfs subvolume create ($mnt)/@"
            ...($sv | each {
                $"btrfs subvolume create ($mnt)/@($in.vol)"
            })

            $"umount ($mnt)"
            ...$ms.root
            $"mkdir -p ($mnt)/{efi,etc,($sv | get dir | str join ',')}"
            ...$ms.boot
            ...$ms.sub
            $"touch ($mnt)/swap/swapfile"
            # Disable COW for this file
            $"chattr +C ($mnt)/swap/swapfile"
            $"chmod 600 ($mnt)/swap/swapfile"
            $"dd if=/dev/zero of=($mnt)/swap/swapfile bs=1G count=($swapsize)"
            $"mkswap -L swap ($mnt)/swap/swapfile"
            ...$ms.swap
        ]
    } else {
        [
            ...$ms.root
            ...$ms.boot
            ...$ms.sub
            ...$ms.swap
        ]
    }
    [...$create_cmd ...$mount_cmd]
}
