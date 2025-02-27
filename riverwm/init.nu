let base = open ~/.config/river/base.nu | from nuon

for i in $base.keymap.normal {
    riverctl map normal $i.mod $i.key ...$i.action
}

for i in $base.keymap.locked {
    riverctl map normal $i.mod $i.key ...$i.action
    riverctl map locked $i.mod $i.key ...$i.action
}

for i in $base.keymap.pointer {
    riverctl map-pointer normal $i.mod $i.key spawn ...$i.action
}

riverctl declare-mode passthrough
riverctl map normal ...$base.passthrough enter-mode passthrough
riverctl map passthrough ...$base.passthrough enter-mode normal

for i in $base.application {
    riverctl map normal $i.mod $i.key spawn ...$i.action
}

for i in $base.setting {
    riverctl ...$i
}

for i in $base.rule {
    riverctl rule-add ...$i
}

for i in $base.execOnce {
    riverctl spawn $"bash -c '([$i '&'] | flatten | str join ' ')'"
}


for i in 0..<8 {
    let tags = 1 | bits shl $i
    riverctl map normal Super $i set-focused-tags $tags
    riverctl map normal Super+Shift $i set-view-tags $tags
    riverctl map normal Super+Control $i toggle-focused-tags $tags
    riverctl map normal Super+Shift+Control $i toggle-view-tags $tags
}

riverctl spawn "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=river"
riverctl spawn "systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=river"

wlr-randr

$env.WLR_NO_HARDWARE_CURSORS = 'true'
$env.LIBVA_DRIVER_NAME = 'nvidia'
$env.__GLX_VENDOR_LIBRARY_NAME = 'nvidia'
$env.GDK_BACKEND = 'wayland,x11,*'
$env.QT_QPA_PLATFORM = 'wayland;xcb'
$env.SDL_VIDEODRIVER = 'wayland'
$env.CLUTTER_BACKEND = 'wayland'

riverctl default-layout rivertile

