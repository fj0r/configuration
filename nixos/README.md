switch to unstable
```
# nix-channel --add https://nixos.org/channels/nixos-unstable nixos
nix-channel --add https://mirrors.ustc.edu.cn/nix-channels/nixpkgs-unstable nixos
nix-channel --add https://mirrors.ustc.edu.cn/nix-channels/nixpkgs-unstable nixpkgs
nix-channel --update
```

install
```
sudo -i

partitions.sh

nixos-generate-config --root /mnt
vim /mnt/etc/nixos/configuration.nix
# If you want to use GRUB, set boot.loader.grub.device to nodev and boot.loader.grub.efiSupport to true.
# nix.settings.substituters = [ "https://mirrors.ustc.edu.cn/nix-channels/store" ];

nixos-install
```

upgrade
```
nixos-rebuild switch --upgrade
```

search
```
nix-env -qaP '.*pip.*'
```
