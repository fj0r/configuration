riverctl map normal Super+Shift E exit
riverctl map normal Super+Shift Return spawn foot
riverctl map normal Super J focus-view next
riverctl map normal Super K focus-view previous
riverctl map normal Super+Shift J swap next
riverctl map normal Super+Shift K swap previous
riverctl map normal Super Period focus-output next
riverctl map normal Super Comma focus-output previous
riverctl map normal Super+Shift Period send-to-output next
riverctl map normal Super+Shift Comma send-to-output previous
riverctl map normal Super Return zoom
riverctl map normal Super H send-layout-cmd rivertile "main-ratio -0.05"
riverctl map normal Super L send-layout-cmd rivertile "main-ratio +0.05"
riverctl map normal Super+Shift H send-layout-cmd rivertile "main-count +1"
riverctl map normal Super+Shift L send-layout-cmd rivertile "main-count -1"
riverctl map normal Super+Alt H move left 100
riverctl map normal Super+Alt J move down 100
riverctl map normal Super+Alt K move up 100
riverctl map normal Super+Alt L move right 100
riverctl map normal Super+Alt+Control H snap left
riverctl map normal Super+Alt+Control J snap down
riverctl map normal Super+Alt+Control K snap up
riverctl map normal Super+Alt+Control L snap right
riverctl map normal Super+Alt+Shift H resize horizontal -100
riverctl map normal Super+Alt+Shift J resize vertical 100
riverctl map normal Super+Alt+Shift K resize vertical -100
riverctl map normal Super+Alt+Shift L resize horizontal 100
riverctl map-pointer normal Super BTN_LEFT move-view
riverctl map-pointer normal Super BTN_RIGHT resize-view
riverctl map-pointer normal Super BTN_MIDDLE toggle-float

for i in 0..<8 {
    let tags = 1 | bits shl $i
}

let config = {
    Q: 'dolphin'
    W: 'qutebrowser'
    E: "nu -c 'source ~/.env.nu; source ~/Configuration/nushell/scripts/main/env.nu; source ~/.nu; cd ~; neovide --frame=none --maximized --vsync'"
    I: 'alacritty'
    O: 'anyrun'
}

$config
| items {|k, v|
    riverctl map normal Super $k spawn $v
}


riverctl background-color 0x002b36
riverctl border-color-focused 0x93a1a1
riverctl border-color-unfocused 0x586e75
riverctl set-repeat 50 300

# Make all views with an app-id that starts with "float" and title "foo" start floating.
riverctl rule-add -app-id 'float*' -title 'foo' float

# Make all views with app-id "bar" and any title use client-side decorations
riverctl rule-add -app-id "bar" csd

# Set the default layout generator to be rivertile and start it.
# River will send the process group of the init executable SIGTERM on exit.
riverctl default-layout rivertile
bash -c "rivertile -view-padding 6 -outer-padding 6 &"

