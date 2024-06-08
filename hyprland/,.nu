### {{{ base.nu
$env.comma_scope = {|_|{ created: '2024-05-10{5}13:48:21' }}
$env.comma = {|_|{}}
### }}}

### {{{ 03_reload.nu
'. reload'
| comma fun {|a,s,_|
    let act = $a | str join ' '
    $', ($act)' | batch -i ',.nu'
} {
    watch: { glob: ",.nu", clear: true }
    completion: {|a,s|
        , -c ...$a
    }
    desc: "reload & run ,.nu"
}
### }}}

'manifest'
| comma val null {
    src: $'($env.HOME)/Downloads/hyprland'
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

'download'
| comma fun {|a,s,_|
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

'setup ubuntu'
| comma fun {
    apt install ...[
        xcb-proto libxcb1 libxcb1-dev
    ]
}

'setup xcberrors'
| comma fun {
    sudo apt install xutils-dev autoconf
    git clone https://gitlab.freedesktop.org/xorg/lib/libxcb-errors --recursive
    cd libxcb-errors
    ./autogen.sh --prefix=/usr
    make install
}

'setup kde'
| comma fun {
    sudo cp plasma-hyprland.sh /usr/local/bin/
    sudo cp plasma-hyprland.desktop /usr/share/xsessions/
}

'install'
| comma fun {|a,s,_|
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

'config'
| comma fun {
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
