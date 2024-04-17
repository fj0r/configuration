let src = $'($env.HOME)/Downloads/hyprland/hyprland'
let dist = '/usr'

let list = {
    Hyprland: bin
    hyprctl: bin
    'libwlroots.so.*': lib
    example/hyprland.desktop: /usr/share/wayland-sessions/
    example/hyprland.conf: $"($env.HOME)/.config/hypr/hyprland.conf"
}

for i in ($list | transpose k v) {
    let s = [$src $i.k] | path join | into glob
    let d = if ($i.v | str starts-with '/') or ($i.v | str starts-with '~') {
        $i.v
    } else {
        [$dist $i.v] | path join
    }
    print $"($s) -> ($d)"
    sudo bash -c $"cp ($s) ($d)"
}

