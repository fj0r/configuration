## work with gui
Windows(宿主机)安装 VcXsrv

```bash
export DISPLAY=$(grep -m 1 nameserver /etc/resolv.conf | awk '{print $2}'):0.0
```

## notifications
```ps
Install-Module -Name BurntToast
```
or
extract `~p/Appilcation/Window/BurntToast.zip` to `$env:userprofile\Documents\WindowsPowerShell\modules\BurntToast`

```bash
toast () { powershell.exe -command New-BurntToastNotification "-Text '$1'" }
```

## Detect "Ubuntu on Windows"
```bash
if grep -q microsoft /proc/version; then
  echo "Ubuntu on Windows"
else
  echo "native Linux"
fi
```

## compile kernel
``` bash
git clone https://github.com/microsoft/WSL2-Linux-Kernel
# check Microsoft/config-wsl
#########################################
# CONFIG_BRIDGE_NETFILTER=y             #
# CONFIG_NETFILTER_XT_MATCH_COMMENT=y   #
# CONFIG_NETFILTER_XT_MATCH_MULTIPORT=y #
# CONFIG_NETFILTER_XT_MATCH_OWNER=y     #
# CONFIG_NETFILTER_XT_MATCH_PHYSDEV=y   #
# CONFIG_VXLAN=y                        #
# CONFIG_GENEVE=y                       #
#########################################
make -j $(nproc) KCONFIG_CONFIG=Microsoft/config-wsl
cp arch/x86/boot/bzImage /mnt/c/tmp/ # ./vmlinux
```

```
wsl.exe --shutdown
cd C:\Windows\System32\lxss\tools
move kernel kernel.back
cp c:\tmp\vmlinux .\kernel

```


## import and export
```
C:\Users\Scott\Desktop> wsl --export Ubuntu-20.04 ./ubuntu.tar
mkdir D:\data\ubuntu
wsl --import ubuntu D:\data\ubuntu ./ubuntu.tar --version 2
wsl --unregister Ubuntu-20.04

# vim /etc/wsl.conf
[user]
default=nash
```

## How to attach other vhdx files to WSL2?
```bash
#Install afflib-tools
sudo apt install afflib-tools

#set some commonly used values:
vhdxFile="/mnt/c/mydisk.vdhx" #Path to VHDX file
mountPoint="/mnt/mydirectory" #Your desired mount point
rawFilePath="${mountPoint}/$(basename "${vhdxFile}".raw)" #do not change this

#Mount the vhdx file
mkdir -p "${mountPoint}"
sudo affuse  "${vhdxFile}" "${mountPoint}"

#Find out the offset where the partition is:
#Example sudo fdisk -l "${rawFilePath}" Output:
#Disk /mnt/mydirectory/mydisk.vmdk.raw: 150 GiB, 161061273600 bytes, 314572800 sectors
#Units: sectors of 1 * 512 = 512 bytes
#Sector size (logical/physical): 512 bytes / 512 bytes
#I/O size (minimum/optimal): 512 bytes / 512 bytes
#Disklabel type: dos
#Disk identifier: 0xe3973aa1

#Device                            Boot Start       End   Sectors  Size Id Type
#/mnt/mydirectory/mydisk.vmdk.raw1 *     2048 164208639 164206592 78.3G 83 Linux

#Multiply the sector size by the partiton Start Value
#In this case 512 from the logical portion the Sector size (3rd line) in the output above
sectorSize=$(sudo fdisk -l "${rawFilePath}" | grep "Sector size" | awk '{print $4}')
#In this case 2048 from the Start number in the final line of the output above
startSector=$(sudo fdisk -l "${rawFilePath}" | tail -n1 | grep -o -E '[0-9]{2,}' | head -n1)
offset=$((${startSector}*${sectorSize}))  #In this case is 1048576

#Now mount the partition you want using the offset (this will replace the original mount which was the full raw disk file)
#Change ro to rw for read/write access instead of read-only access
sudo mount -o ro,loop,offset=${offset} "${rawFilePath}" "${mountPoint}"

#Done
echo "VHDx Disk [${vhdxFile}] mounted at [${mountPoint}] is now accessible in windows from [\\\\wsl$\Ubuntu${mountPoint////\\}], this includes disks with EXTx filesystems"
```