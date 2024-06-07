mut manifest = {}

## setup
let apps = [
    {name: nushell}
    {name: neovim}
    {name: neovide}
    {name: alacritty}
    {name: git, tag: [dev]}
    {name: ripgrep, tag: [dev]}
    {name: jq, tag: [dev]}
    {name: fd, tag: [dev]}
    {name: curl, tag: [network dev]}
    {name: podman, tag: container}
    {name: buildah, tag: container}
    {name: skopeo, tag: container}
    {name: wireguard-tools, tag: [network]}
    {name: tree}
    {name: wget}
    {name: sqlite}
    {name: xclip}
    #{name: x11-utils}
    {name: vivaldi}
    {name: hyprland}
    {name: dust}
]

$manifest.packages = {
    message: 'install packages'
    action: {
        sudo pacman -S ...($apps | get name)
    }
}


$manifest.podman = {
    message: ''
    action: {
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
}

$manifest.ctrlcaps = {
    message: 'swap ctrl and caps'
    action: {
        localectl set-x11-keymap "" "" "" ctrl:swapcaps
    }
}

$manifest.nopasswd = {
    message: 'sudo without password'
    action: {
        # sudo sed -i 's/^.*\(%wheel.*\)NOPASSWD: ALL$/\1NOPASSWD: ALL/g' /etc/sudoers.d/10-installer
        '%wheel ALL=(ALL) NOPASSWD: ALL' | sudo tee /etc/sudoers.d/g_wheel
    }
}

$manifest.loader_timeout = {
    message: ''
    action: {
        sudo sed -i 's/^timeout.*/timeout 1/g' /efi/loader/loader.conf
    }
}

$manifest.disable_beep = {
    message: 'disable system beep'
    action: {
        sudo rmmod pcspkr
        'blacklist pcspkr' | sudo tee -a /etc/modprobe.d/nobeep.conf
    }
}

$manifest.wireguard = {
    message: ''
    action: {
        for c in (ls ~/.ssh/wg* | get name) {
            sudo cp $c /etc/wireguard/
            ssc enable --now $"wg-quick@($c | path parse | get stem)"
        }
    }
}

$manifest.python = {
    message: ''
    action: {
        sudo pacman -S python python-pip
        pip install --no-cache-dir --break-system-packages ...[
            aiofile fastapi uvicorn
            ipython debugpy pydantic pytest
            httpx hydra-core typer pyyaml deepmerge
            PyParsing structlog python-json-logger
            decorator more-itertools cachetools
        ]
    }
}

$manifest.rust = {
    action: {
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
}

$manifest.haskell = {
    action: {
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
}

$manifest.lsp = {
    message: 'language server'
    action: {
        sudo pacman -S nodejs npm
        sudo npm install --location=global ...[
            quicktype
            pyright
            vscode-langservers-extracted
            yaml-language-server
        ]

        let lua_ls = curl --retry 3 -sSL https://api.github.com/repos/LuaLS/lua-language-server/releases/latest | jq -r '.tag_name'
        let lua_ls = $"https://github.com/LuaLS/lua-language-server/releases/latest/download/lua-language-server-($lua_ls)-linux-x64.tar.gz"
        sudo mkdir -p $"($env.LS_ROOT)/lua"
        curl --retry 3 -sSL $lua_ls | sudo tar zxf - -C $"($env.LS_ROOT)/lua"
    }
}
