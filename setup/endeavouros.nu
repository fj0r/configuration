mut manifest = {}
mut actions = {}

## setup
let apps = [
    { name: nushell, tag: [core] }
    { name: neovim, tag: [core] }
    { name: neovide, tag: [core dev] }
    { name: alacritty, tag: [core dev] }
    { name: git, tag: [core dev] }
    { name: ripgrep, tag: [core] }
    { name: jq, tag: [core] }
    { name: fd, tag: [core] }
    { name: curl, tag: [core network] }
    { name: dust, tag: [core] }
    { name: rustic, tag: [core backup] }
    { name: freefilesync, aur: 'freefilesync-bin', tag: [backup] }
    { name: podman, tag: [container] }
    { name: buildah, tag: [container] }
    { name: skopeo, tag: [container] }
    { name: wireguard-tools, tag: [core vpn] }
    { name: tree, tag: [core] }
    { name: wget, tag: [network] }
    { name: sqlite, tag: [core] }
    { name: xclip, tag: [core] }
    { name: kubectl, tag: [k8s] }
    { name: helm, tag: [k8s] }
    # {name: x11-utils}
    { name: yay, tag: [core] }
    { name: vivaldi, tag: [core network] }
    { name: hyprland, aur: 'hyprland-nvidia-git', tag: [wm] }
    { name: waybar, tag: [wm] }
    { name: mako, tag: [wm] }
    { name: wofi, tag: [wm] }
]

$manifest.packages = {
    message: 'install packages'
}
$actions.packages = {
    sudo pacman -S ...($apps | get name)
}


$manifest.podman = {
    message: ''
}
$actions.podman = {
    if ($apps | where name == 'podman' | is-not-empty) {
        print -e 'setup podman'
        '
        unqualified-search-registries = ["docker.io"]
        [[registry]]
        insecure = true
        location = "registry.s"
        '
        | outdent
        | sudo tee -a /etc/containers/registries.conf

        sudo sed ...[
            -e 's!^.*\(detach_keys =\).*$!\1 ""!'
            -e 's!^.*\(multi_image_archive =\).*$!\1 true!'
            -i /usr/share/containers/containers.conf
        ]
    } else {
        print 'podman not found'
    }
}

$manifest.ctrlcaps = {
    message: 'swap ctrl and caps'
}
$actions.ctrlcaps = {
    localectl set-x11-keymap "" "" "" ctrl:swapcaps
}

$manifest.nopasswd = {
    message: 'sudo without password'
}
$actions.nopasswd = {
    # sudo sed -i 's/^.*\(%wheel.*\)NOPASSWD: ALL$/\1NOPASSWD: ALL/g' /etc/sudoers.d/10-installer
    '%wheel ALL=(ALL) NOPASSWD: ALL' | sudo tee /etc/sudoers.d/g_wheel
}

$manifest.loader_timeout = {
    message: ''
}
$actions.loader_timeout = {
    sudo sed -i 's/^timeout.*/timeout 1/g' /efi/loader/loader.conf
}

$manifest.disable_beep = {
    message: 'disable system beep'
}
$actions.disable_beep = {
    sudo rmmod pcspkr
    'blacklist pcspkr' | sudo tee -a /etc/modprobe.d/nobeep.conf
}

$manifest.wireguard = {
    message: ''
}
$actions.wireguard = {
    for c in (ls ~/.ssh/wg* | get name) {
        sudo cp $c /etc/wireguard/
        ssc enable --now $"wg-quick@($c | path parse | get stem)"
    }
}

$manifest.mirrors = {
    message: ''
}
$actions.mirrors = {
    [
        'Server = https://mirrors.tuna.tsinghua.edu.cn/archlinux/$repo/os/$arch'
        'Server = https://mirrors.ustc.edu.cn/archlinux/$repo/os/$arch'
        (open /etc/pacman.d/mirrorlist)
    ]
    | str join (char newline)
    | sudo tee /etc/pacman.d/mirrorlist.backup

    let countries = [cn hk jp sg kr tw] # us
    | each {|x| $"country=($x | str upcase)" }
    | str join '&'
    curl $"https://archlinux.org/mirrorlist/?($countries)&protocol=http&use_mirror_status=on"
    | sed -e 's/^#Server/Server/' -e '/^#/d'
    | rankmirrors -n 5 -

    reflector-simple

}

$manifest.python = {
    message: ''
}
$actions.python = {
    sudo pacman -S python python-pip
    pip install --no-cache-dir --break-system-packages ...[
        aiofile fastapi uvicorn
        ipython debugpy pydantic pytest
        httpx hydra-core typer pyyaml deepmerge
        PyParsing structlog python-json-logger
        decorator more-itertools cachetools
    ]
}

$manifest.rust = {
}
$actions.rust = {
    curl --retry 3 -sSL https://sh.rustup.rs
    | sh -s -- --default-toolchain stable -y --no-modify-path
    rustup component add rust-src clippy rustfmt
    rustup component add rust-analyzer
    rustup target add x86_64-unknown-linux-musl
    rustup target add wasm32-wasi wasm32-unknown-unknown
    cargo install ...[
        cargo-wasi
        cargo-watch cargo-expand cargo-eval cargo-tree
        cargo-feature cargo-prefetch
        cargo-generate
        dioxus-cli
    ]
}

$manifest.haskell = {
}
$actions.haskell = {
    # $env.BOOTSTRAP_HASKELL_NONINTERACTIVE = 1
    $env.GHCUP_ROOT = $"($env.HOME)/.ghcup"
    $env.STACK_ROOT = $"($env.HOME)/.stack"
    curl --retry 3 -sSLo $"($env.GHCUP_ROOT)/bin/ghcup" https://downloads.haskell.org/~ghcup/x86_64-linux-ghcup
    ghcup install stack
    ghcup install cabal
    stack config set system-ghc --global true
    stack config set install-ghc --global false
    let ghc_ver = curl --retry 3 -sSL https://www.stackage.org/lts -H 'Accept: application/json' | jq -r '.snapshot.ghc'
    ghcup -s '["GHCupURL", "StackSetupURL"]' install ghc $ghc_ver
    ghcup install hls
    for i in [tmp cache trash logs] {
        rm -rf ($"($env.GHCUP_ROOT)/($i)/*" | into glob)
    }
    open $"($env.STACK_ROOT)/config.yaml"
    | upsert allow-different-user true
    | upsert allow-newer true
    | save -f $"($env.STACK_ROOT)/config.yaml"
}

$manifest.lsp = {
    message: 'language server'
}
$actions.lsp = {
    sudo pacman -S nodejs npm
    sudo npm install --location=global ...[
        quicktype
        pyright
        vscode-langservers-extracted
        yaml-language-server
    ]

    let lua_ls = curl --retry 3 -sSL https://api.github.com/repos/LuaLS/lua-language-server/releases/latest | jq -r '.tag_name'
    let lua_ls = $"https://github.com/LuaLS/lua-language-server/releases/latest/download/lua-language-server-($lua_ls)-linux-x64.tar.gz"
    sudo mkdir -p $"($env.LS_ROOT)/lua/log"
    sudo chmod 777 /opt/language-server/lua/log
    curl --retry 3 -sSL $lua_ls | sudo tar zxf - -C $"($env.LS_ROOT)/lua"
}

$manifest.fcitx = {
    message: ''
}
$actions.fcitx = {
    sudo pacman -S ...[
        fcitx5 fcitx5-rime fcitx5-lua rime-wubi
        fcitx5-qt fcitx5-gtk fcitx5-nord
    ]
    [
        'INPUT_METHOD=fcitx'
        'XMODIFIERS=@im=fcitx'
        #'GTK_IM_MODULE=fcitx'
        #'QT_IM_MODULE=fcitx'
        #'SDL_IM_MODULE=fcitx'
    ]
    | str join (char newline)
    | sudo tee -a /etc/environment
}

$manifest.kde = {
    message: ''
}
$actions.kde = {
    [
        '[Windows]'
        'BorderlessMaximizedWindows=true'
    ]
    | str join (char newline)
    | sudo tree -a ~/.config/kwinrc
}
