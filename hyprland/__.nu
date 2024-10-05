const s = {
    manifest: {
        src: $'~/Downloads/hyprland'
        dist: '/usr'
        list: {
            Hyprland: 'bin/'
            hyprctl: 'bin/'
            hyprpm: 'bin/'
            'example/hyprland.desktop': 'share/wayland-sessions/'
            'example/hyprland.conf': 'share/hyprland/'
            'assets/hyprland-portals.conf': 'share/xdg-desktop-portal/'
            'assets/wall*': 'share/hyprland/'
        }
    }
}

export def 'download' [] {
    let src = $s.manifest.src
    let src1 = $src | path split | range ..-2 | path join
    let v = curl --retry 3 -sSL https://api.github.com/repos/Hyprwm/Hyprland/releases/latest
    | from json
    | get tag_name
    let url = $"https://github.com/hyprwm/Hyprland/releases/download/($v)/($v).tar.gz"
    mkdir $src
    lg level 1 {url: $url, dist: $src} download
    curl -sSL $url | tar zxf - -C $src1
}

export def 'setup ubuntu' [] {
    apt install ...[
        xcb-proto libxcb1 libxcb1-dev
    ]
}

export def 'setup xcberrors' [] {
    sudo apt install xutils-dev autoconf
    git clone https://gitlab.freedesktop.org/xorg/lib/libxcb-errors --recursive
    cd libxcb-errors
    ./autogen.sh --prefix=/usr
    make install
}

export def 'setup kde' [] {
    sudo cp plasma-hyprland.sh /usr/local/bin/
    sudo cp plasma-hyprland.desktop /usr/share/xsessions/
}

export def 'install' [] {
    let m = $s.manifest
    for i in ($m.list | transpose k v) {
        let s = [$m.src $i.k] | path join | into glob
        let d = if ($i.v | str starts-with '/') or ($i.v | str starts-with '~') {
            $i.v
        } else {
            [$m.dist $i.v] | path join
        }
        if ($d | path exists | not $in) {
            pp sudo mkdir $d
        }
        pp sudo bash -c $"cp ($s) ($d)"
    }
}

export def 'config' [] {
    let d = $"($env.HOME)/.config/hypr/hyprland.conf"
    if ($d | path exists) {
        if ([y n] | input list 'exists! delete?') == 'y' {
            rm -f $d
        } else {
            return
        }
    }
    cp hyprland.conf $d
}
