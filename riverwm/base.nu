{
    application: [
        [mod, key, action];
        [Super Q [dolphin]]
        [Super W [qutebrowser]]
        [Super E ["nu -c 'source ~/.env.nu; source ~/Configuration/nushell/scripts/main/env.nu; source ~/.nu; cd ~; neovide --maximized'"]]
        [Super I [alacritty]]
        [Super O [walker]]
        [Super R [`bash -c 'grim -g "$(slurp)" - | wl-copy && wl-paste > ~/Pictures/Screenshots/Screenshot-$(date +%F_%T).png | dunstify "Screenshot of the region taken" -t 1000'`]]
    ]
    execOnce: [
        [rivertile -view-padding 0 -outer-padding 0]
        [mako]
        [fcitx5 -d]
        #[localectl set-x11-keymap "" "" "" ctrl:swapcaps]
        #[setxkbmap -option 'ctrl:swapcaps']
    ]
    env: {
        INPUT_METHOD: fcitx
        XMODIFIERS: @im=fcitx
    }
    rule: [
        [-app-id 'float*' -title 'foo' float]
        [-app-id "bar" csd]
    ]
    passthrough: [Super F11]
    setting: [
        [background-color '0x002b36']
        [border-color-focused '0x33ccff']
        [border-color-unfocused '0x586e75']
        #[set-repeat 50 300]
        #[xcursor-theme capitaine-cursors]
    ]
    keymap: {
        normal: [
            [mod, key, action];
            [Alt Tab [zoom]]
            [Super+Shift E [exit]]
            [Super J [focus-view next]]
            [Super K [focus-view previous]]
            [Super+Shift J [swap next]]
            [Super+Shift K [swap previous]]
            [Super Period [focus-output next]]
            [Super Comma [focus-output previous]]
            [Super+Shift Period [send-to-output next]]
            [Super+Shift Comma [send-to-output previous]]
            [Super H [send-layout-cmd rivertile "main-ratio -0.05"]]
            [Super L [send-layout-cmd rivertile "main-ratio +0.05"]]
            [Super+Shift H [send-layout-cmd rivertile "main-count +1"]]
            [Super+Shift L [send-layout-cmd rivertile "main-count -1"]]
            [Super+Alt H [move left 100]]
            [Super+Alt J [move down 100]]
            [Super+Alt K [move up 100]]
            [Super+Alt L [move right 100]]
            [Super+Alt+Control H [snap left]]
            [Super+Alt+Control J [snap down]]
            [Super+Alt+Control K [snap up]]
            [Super+Alt+Control L [snap right]]
            [Super+Alt+Shift H [resize horizontal -100]]
            [Super+Alt+Shift J [resize vertical 100]]
            [Super+Alt+Shift K [resize vertical -100]]
            [Super+Alt+Shift L [resize horizontal 100]]
            [Super F [toggle-float]]
            [Super+Shift F [toggle-fullscreen]]
        ]
        locked: [
            [mod, key, action];
            [None XF86Eject ['eject -T']]
            # Control pulse audio volume with pamixer (https://github.com/cdemoulins/pamixer)
            [None XF86AudioRaiseVolume  ['pamixer -i 5']]
            [None XF86AudioLowerVolume  ['pamixer -d 5']]
            [None XF86AudioMute         ['pamixer --toggle-mute']]
            # Control MPRIS aware media players with playerctl (https://github.com/altdesktop/playerctl)
            [None XF86AudioMedia ['playerctl play-pause']]
            [None XF86AudioPlay  ['playerctl play-pause']]
            [None XF86AudioPrev  ['playerctl previous']]
            [None XF86AudioNext  ['playerctl next']]
            # Control screen backlight brightness with brightnessctl (https://github.com/Hummer12007/brightnessctl)
            [None XF86MonBrightnessUp   ['brightnessctl set +5%']]
            [None XF86MonBrightnessDown ['brightnessctl set 5%-']]
        ]
        pointer: [
            [mod, key, action];
            [Super BTN_LEFT [move-view]]
            [Super BTN_RIGHT [resize-view]]
            [Super BTN_MIDDLE [toggle-float]]
        ]
    }
}
