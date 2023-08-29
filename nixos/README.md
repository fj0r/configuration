
```
sudo -i

partitions.sh

# nixos-generate-config --root /mnt
# vim /mnt/etc/nixos/configuration.nix
## If you want to use GRUB, set boot.loader.grub.device to nodev and boot.loader.grub.efiSupport to true.
## nix.settings.substituters = [ "https://mirrors.ustc.edu.cn/nix-channels/store" ];

# nix-channel --add https://mirrors.ustc.edu.cn/nix-channels/nixos-23.05 nixos
# nix-channel --update

# nixos-install
```
