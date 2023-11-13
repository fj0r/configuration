```
setxkbmap -option 'ctrl:swapcaps'
localectl set-x11-keymap "" "" "" ctrl:swapcaps
```


### debian
```
sudo vi /etc/default/keyboard
```

change `XKBOPTIONS=""` to `XKBOPTIONS="ctrl:swapcaps"`

```
sudo dpkg-reconfigure keyboard-configuration
```
