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
        - [x] realtime tooltip
    - [x] clock tooltip
    - [x] disk
    - [x] margin
- overseer tag
    - [ ] rules

```lua
{
  rule = { class = "wm_kybrd_fcns.py" },
  properties = { floating = true },
  screen = awful.screen.focused,
  tags = { "1", "2", "3", "4", "5" }
},
```
