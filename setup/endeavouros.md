## disable system beep
sudo rmmod pcspkr
sudo nvim /etc/modprobe.d/nobeep.conf
```
blacklist pcspkr
```
