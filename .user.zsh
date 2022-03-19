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

#source /home/nash/k8s/istio-*/tools/_istioctl


[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

alias ytd="/opt/youtube-dl/youtube-dl --proxy http://localhost:7890"
alias ytdm="ytd -x --audio-quality 0 "
alias xr="source ~/.config/xrandr.rc"

function github_version {
    github_header="Accept: application/vnd.github.v3+json"
    github_api=https://api.github.com/repos
    curl -sSL -H $github_header $github_api/${1}/releases | jq -r '.[].tag_name'
}

function archive-cfg {
    local d=$(date +"%Y%m%d%H%M%S")
    local tmp="/tmp/cfg/$d/home"
    mkdir -p $tmp
    cp -r $CFG/../.zshrc.d $tmp/.zshrc.d
    ln -fsr $tmp/.zshrc.d/_zshrc $tmp/.zshrc
    cp $CFG/../.ext.zsh $tmp/
    mkdir -p $tmp/.config
    cp $CFG/../_tmux.conf $tmp/.tmux.conf
    mkdir -p $tmp/.config/helix
    cp $CFG/../helix/* $tmp/.config/helix
    mkdir -p $tmp/.local/bin
    cp /usr/local/bin/{just,watchexec,rq,yq,rg,fd,sd,dust,btm,xh,dog} $tmp/.local/bin
    #tar hzcvf - --transform "s|^$d\(.*\)|\1|" -C /tmp/cfg $d
    # cp -r $CFG/nvim $tmp/.config/
    #tar \
    #        --exclude='*/.git*' \
    #        --exclude='*/__pycache__*' \
    #        --exclude='*/nvim-luapad/gifs*' \
    #        --exclude='*/plugged/ultisnips/doc/*' \
    #        --exclude='*/plugged/vimspector/gadgets/*' \
    #        --exclude='plugged/LeaderF/autoload/leaderf/fuzzyMatch_C/build' \
    #        -cf - -C $CFG nvim | tar -xf - -C $tmp/.config
    #rm -f $tmp/.config/nvim/.netrwhist
    pushd $tmp/..
    tar -zcvf cfg.tgz home
    rm -f $HOME/pub/cfg.tgz
    mv cfg.tgz $HOME/pub
    popd
    rm -rf /tmp/cfg
    echo "restore: cat $HOME/pub/cfg.tgz | tar -C ~ -zxvf - --strip-component=1"
}

function archive-nvim-cfg {
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
    for i in nvim `sh -c 'ls -d nvim/pack/packer/start/*/'` `sh -c 'ls -d nvim/pack/packer/opt/*/'`; do
        pushd $i
        echo ----------------------
        pwd
        git reflog expire --all --expire=now && git gc --prune=now --aggressive
        popd
    done
    tar \
        --exclude='pack/packer/start/*/.git' \
        --exclude='pack/packer/opt/*/.git' \
        --exclude='.git' \
        -zcf nvim-cfg.tar.gz nvim
    rm -f $HOME/pub/nvim-cfg.tar.gz
    mv nvim-cfg.tar.gz $HOME/pub
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
    local cmd="cat $HOME/pub/cfg.tgz "
    local sshcmd="ssh"
    for i in $*; do
        echo "--------- to $i"
        cmd+="| tee >($sshcmd $i \"rm -rf ~/.zshrc.d; tar zxf - --strip-component=1; sudo chown \\\$(id -u):\\\$(id -g) -R ~/{.zshrc,.zshrc.d}\") "
    done
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd

    echo "========= deploy neovim"
    local cmd="cat $HOME/Downloads/nvim-linux64.tar.gz "
    for i in $*; do
        echo "--------- to $i"
        cmd+="| tee >($sshcmd $i \"sudo tar zxf - -C /usr/local/ --strip-components=1\")"
    done
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd

    echo "========= config neovim"
    local cmd="cat $HOME/pub/nvim-cfg.tar.gz "
    for i in $*; do
        echo "--------- to $i"
        cmd+="| tee >($sshcmd $i \"rm -rf ~/.config/nvim; tar zxf - -C ~/.config; sudo chown \\\$(id -u):\\\$(id -g) -R ~/.config/nvim\")"
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


