## dependencies
- nvim
- wezterm
- rofi
- scrot
- xscreensaver
- wallpaper(~/Pictures/wallpaper)

## todo
- monitor
    - [x] dual graph
        - [ ] realtime tooltip
    - [x] clock tooltip
    - [ ] disk
- overseer tag

```lua
{
  rule = { class = "wm_kybrd_fcns.py" },
  properties = { floating = true },
  screen = awful.screen.focused,
  tags = { "1", "2", "3", "4", "5" }
},
```
