export http_proxy=http://localhost:7890
export https_proxy=http://localhost:7890
export NVIM_PRESET=full
export VIM_DUAL_ESC=0
export KUBERNETES_SCHEMA_URL=file://$HOME/world/kubernetes-json-schema/all.json

edit-rime () {
    pushd ~/data/rime-wubi
    nvim wubi86_fg.dict.yaml
}

if (( $+commands[zoxide] )); then
    eval "$(zoxide init zsh)"
    alias cd=z
fi

if [ -n "$WSL_DISTRO_NAME" ]; then
  #export DISPLAY=${route}:0.0
  #export WSLHOME=$(wslpath $(wslvar USERPROFILE))
fi

# docker save xxx | r='docker load'; parallel-ssh a b c


installNeovim () {
    if [ -z "$1" ]; then
        cat ~/nvim-linux64.tar.gz | sudo tar zxf - -C /usr/local --strip-components=1
    else
        curl -# -SL https://github.com/neovim/neovim/releases/download/${NVIM_VERSION:-nightly}/nvim-linux64.tar.gz \
          |sudo tar zxf - -C /usr/local --strip-components=1
    fi
}

function parallel-ssh {
    local cmd=""
    for i in $*
        cmd+="| tee >(ssh $i '${r}') "
    cmd=${cmd:1}
    cmd+="> /dev/null"
    echo $cmd
}
compdef parallel-ssh=ssh


export PATH=$HOME/anaconda3/bin:$PATH


function efav {
    entf nash@iffy.me "⭐️$1" $2
}

hash -d tut="$HOME/pub/Tutorial"
hash -d plt="$HOME/pub/Platform"
hash -d dk="$HOME/data/docker.io"
hash -d k8s="$HOME/data/k8s"
hash -d a="$CFG/../awesome"

#source /home/nash/k8s/istio-*/tools/_istioctl


[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

alias ytd="youtube-dl --proxy http://localhost:7890"
alias ytdm="ytd -x --audio-quality 0 "
function xr {
    source ~/.config/xrandr.rc
    sudo rm -rf /var/log/journal/*
}

function github_version {
    github_header="Accept: application/vnd.github.v3+json"
    github_api=https://api.github.com/repos
    curl -sSL -H $github_header $github_api/${1}/releases | jq -r '.[].tag_name'
}

function archive-cfg-nvim {
    local d=$(date +"%Y%m%d%H%M%S")
    local tmp="/tmp/cfg/$d"
    mkdir -p $tmp
    tar \
            --exclude='*/__pycache__*' \
            --exclude='*/nvim-luapad/gifs*' \
            --exclude='*/plugged/ultisnips/doc/*' \
            --exclude='*/plugged/vimspector/gadgets/*' \
            --exclude='plugged/LeaderF/autoload/leaderf/fuzzyMatch_C/build' \
            --exclude='pack/packer/*/*/.github' \
            --exclude='pack/packer/*/*/.git' \
            --exclude='.git*' \
            -cf - -C $CFG/.. nvim | tar -xf - -C $tmp/
    rm -f $tmp/nvim/.netrwhist
    pushd $tmp
    tar \
        -zcf cfg.tar.gz nvim
    rm -f $HOME/pub/nvim-cfg.tar.gz
    mv cfg.tar.gz $HOME/pub/nvim-cfg.tar.gz
    popd
    rm -rf $tmp
}

function archive-cfg {
    local d=$(date +"%Y%m%d%H%M%S")
    local tmp="/tmp/cfg/$d"
    mkdir -p $tmp
    tar \
            --exclude='*/__pycache__*' \
            --exclude='*/nvim-luapad/gifs*' \
            --exclude='*/plugged/ultisnips/doc/*' \
            --exclude='*/plugged/vimspector/gadgets/*' \
            --exclude='plugged/LeaderF/autoload/leaderf/fuzzyMatch_C/build' \
            --exclude='pack/packer/*/nvim-tree.lua/.github/*' \
            -cf - -C $CFG/.. nvim | tar -xf - -C $tmp/
    rm -f $tmp/nvim/.netrwhist
    pushd $tmp
    cp -r $CFG/../nushell .
    cp -r $CFG/../tmux .
    for i in nvim `sh -c 'ls -d nvim/pack/packer/start/*/'` `sh -c 'ls -d nvim/pack/packer/opt/*/'`; do
        pushd $i
        echo ----------------------
        pwd
        git reflog expire --all --expire=now && git gc --prune=now --aggressive
        popd
    done
    tar \
        -zcf cfg.tar.gz nvim nushell tmux
    rm -f $HOME/pub/cfg.tar.gz
    mv cfg.tar.gz $HOME/pub
    popd
    rm -rf $tmp
}

function deploy-to-server {
    #rsync -av -e ssh $rc $1:~/.zshrc
    #rsync -av --delete -e ssh $CFG/.zshrc.d/ $1:~/.zshrc.d
    #rsync -av -e ssh $CFG/.ext.zsh $1:~/.ext.zsh
    #rsync -av -e ssh $CFG/.kubectl.zsh $1:~/.kubectl.zsh
    #rsync -av -e ssh $CFG/_tmux.conf $1:~/.tmux.conf
    #rsync -av --delete -e ssh $CFG/.fzf/ $1:~/.fzf
    echo "========= deploy config"
    local cmd="cat $HOME/pub/cfg.tar.gz "
    local sshcmd="ssh"
    for i in $*; do
        echo "--------- to $i"
        cmd+="| tee >($sshcmd $i \"rm -rf ~/.config/{nushell,nvim,tmux}; tar zxf - -C ~/.config; sudo chown \\\$(id -u):\\\$(id -g) -R ~/.config/{nushell,nvim,tmux}\") "
    done
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd

    echo "========= deploy app"
    local cmd="cat $HOME/Downloads/nvim-linux64.tar.gz "
    for i in $*; do
        echo "--------- to $i"
        cmd+="| tee >($sshcmd $i \"sudo tar zxf - -C /usr/local/ --strip-components=1\")"
    done
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd

    local cmd="cat $HOME/Downloads/nu-0.64.0-x86_64-unknown-linux-musl.tar.gz "
    for i in $*; do
        echo "--------- to $i"
        cmd+="| tee >($sshcmd $i \"sudo tar zxf - -C /usr/local/bin/ --wildcards 'nu*'\")"
    done
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd
    
}
compdef deploy-to-server=ssh

function upload-docker-images {
    local cmd="docker save $images | zstd -c -T0 -8 "
    for i in $*
        cmd+="| tee >(ssh $i 'zstd -d | docker load') "
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd
}
compdef upload-docker-images


