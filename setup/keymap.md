```
# setxkbmap -option 'caps:swapescape,ctrl:swap_lalt_lctl,ctrl:swap_ralt_rctl'
setxkbmap -option 'ctrl:swapcaps'
localectl set-x11-keymap "" "" "" ctrl:swapcaps
```

## option
|      option              |      descripton                                          |
|--------------------------|----------------------------------------------------------|
| ctrl:lctrl_meta          | Left Ctrl as Meta                                        |
| ctrl:swapcaps            | Swap Ctrl and Caps Lock                                  |
| ctrl:hyper_capscontrol   | Caps Lock as Ctrl, Ctrl as Hyper                         |
| ctrl:swap_lalt_lctl      | Swap Left Alt with Left Ctrl                             |
| ctrl:swap_ralt_rctl      | Swap Right Alt with Right Ctrl                           |
| ctrl:swap_lwin_lctl      | Swap Left Win with Left Ctrl                             |
| ctrl:swap_rwin_rctl      | Swap Right Win with Right Ctrl                           |
| ctrl:swap_lalt_lctl_lwin | Left Alt as Ctrl, Left Ctrl as Win, Left Win as Left Alt |
| caps:swapescape          | Swap Esc and Caps Lock                                   |
| caps:escape              | Make Caps Lock an additional Esc                         |

> /usr/share/X11/xkb/rules/base.lst

### debian
```
sudo vi /etc/default/keyboard
```

change `XKBOPTIONS=""` to `XKBOPTIONS="ctrl:swapcaps"`

```
sudo dpkg-reconfigure keyboard-configuration
```
