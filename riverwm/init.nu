let keymap = [
    [mod, key, map];
    [Super Q [dolphin]]
    [Super W [qutebrowser]]
    [Super E ["nu -c 'source ~/.env.nu; source ~/Configuration/nushell/scripts/main/env.nu; source ~/.nu; cd ~; neovide --frame=none --maximized --vsync'"]]
    [Super I [alacritty]]
    [Super O [anyrun]]
]

let base = open ~/.config/river/base.nu | from nuon

for i in ($base.keymap | append $keymap) {
    riverctl map normal $i.mod $i.key ...$i.map
}

for i in ($base.pointer) {
    riverctl map-pointer normal $i.mod $i.key ...$i.map
}

for i in ($base.setting) {
    riverctl ...$i
}

for i in ($base.rule) {
    riverctl rule-add ...$i
}

for i in ($base.execOnce) {
    bash -c ([$i '&'] | flatten | str join ' ')
}


for i in 0..<8 {
    let tags = 1 | bits shl $i
}

riverctl default-layout rivertile

