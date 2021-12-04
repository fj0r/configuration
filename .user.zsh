export http_proxy=http://localhost:7890
export https_proxy=http://localhost:7890
export NVIM_PRESET=full
export VIM_DUAL_ESC=0
export KUBERNETES_SCHEMA_URL=file://$HOME/world/kubernetes-json-schema/all.json
export PATH=/opt/kak-lsp:/opt/helix:/opt/julia/bin:/opt/ghc/bin:$PATH

if (( $+commands[zoxide] )); then
    eval "$(zoxide init zsh)"
    alias cd=z
fi

if [ -n "$WSL_DISTRO_NAME" ]; then
  #export DISPLAY=${route}:0.0
  #export WSLHOME=$(wslpath $(wslvar USERPROFILE))
fi

# docker save xxx | r='docker load'; parallel-ssh a b c

export PATH=/opt/nu:$PATH

alias lg='lazygit'
alias kk='/usr/bin/kak'
alias nv='nvim -u $CFG/nvim/init.vim'
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


export PATH=$HOME/anaconda3/bin:/opt/blender:$PATH

source ~/.email.token

function efav {
    entf nash@iffy.me "⭐️$1" $2
}

hash -d tut="$HOME/pub/Tutorial"
hash -d plt="$HOME/pub/Platform"

#source /home/nash/k8s/istio-*/tools/_istioctl


[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

alias ytd="youtube-dl --proxy http://localhost:7890"
alias ytdm="ytd -x --audio-quality 0 "

function github_version {
    github_header="Accept: application/vnd.github.v3+json"
    github_api=https://api.github.com/repos
    curl -sSL -H $github_header $github_api/${1}/releases | jq -r '.[].tag_name'
}

function archive-cfg-coc {
    local d=$(date +"%Y%m%d%H%M%S")
    local tmp="/tmp/cfg/$d/home"
    mkdir -p $tmp
    cp $CFG/_zshrc $tmp/.zshrc
    cp -r $CFG/.zshrc.d $tmp/.zshrc.d
    cp $CFG/.ext.zsh $tmp/
    mkdir -p $tmp/.config
    cp $CFG/_tmux.conf $tmp/.tmux.conf
    mkdir -p $tmp/.local/bin
    cp /usr/local/bin/{just,watchexec,yq,rg} $tmp/.local/bin
    #tar hzcvf - --transform "s|^$d\(.*\)|\1|" -C /tmp/cfg $d
    # cp -r $CFG/nvim $tmp/.config/
    pushd $CFG
    _exclude_coc=$(find nvim-coc/coc-data -mindepth 1 -maxdepth 1 ! -wholename nvim-coc/coc-data/extensions)
    _exclude_coc_s=$(find nvim-coc/coc-data/extensions -mindepth 1 -maxdepth 1 ! -wholename nvim-coc/coc-data/extensions/node_modules ! -wholename nvim-coc/coc-data/extensions/package.json ! -wholename nvim-coc/coc-data/extensions/coc-lua-data)
    _exclude_coc_exts=$(find nvim-coc/coc-data/extensions/node_modules -mindepth 1 -maxdepth 1 -type d  $(printf "! -wholename nvim-coc/coc-data/extensions/node_modules/coc-%s " $(cat nvim-coc/coc-core-extensions)))
    lst1=$(jq -nR '[inputs|select(length>0)]|map("coc-"+.)' nvim-coc/coc-core-extensions)
    lst2=$(cat nvim-coc/coc-data/extensions/package.json | jq '.dependencies|keys' | jq ". - $lst1")
    pkglist=$(cat nvim-coc/coc-data/extensions/package.json| jq "del(.dependencies${lst2})")
    popd
    tar \
            --exclude-from=<(echo "$_exclude_coc") \
            --exclude-from=<(echo "$_exclude_coc_s") \
            --exclude-from=<(echo "$_exclude_coc_exts") \
            --exclude='*/.git*' \
            --exclude='*/__pycache__*' \
            --exclude='*/nvim-luapad/gifs*' \
            --exclude='*/plugged/ultisnips/doc/*' \
            --exclude='*/plugged/vimspector/gadgets/*' \
            --exclude='plugged/LeaderF/autoload/leaderf/fuzzyMatch_C/build' \
            -cf - -C $CFG nvim-coc | tar -xf - -C $tmp/.config
    rm -f $tmp/.config/nvim-coc/.netrwhist
    mv $tmp/.config/nvim-coc $tmp/.config/nvim
    echo $pkglist > $tmp/.config/nvim/coc-data/extensions/package.json
    pushd $tmp/..
    tar -zcvf cfg.tgz home
    rm -f $HOME/pub/cfg-coc.tgz
    mv cfg.tgz $HOME/pub/cfg-coc.tgz
    popd
    rm -rf /tmp/cfg
    echo "restore: cat $HOME/pub/cfg.tgz | tar -C ~ -zxvf - --strip-component=1"
}

function archive-cfg {
    local d=$(date +"%Y%m%d%H%M%S")
    local tmp="/tmp/cfg/$d/home"
    mkdir -p $tmp
    cp $CFG/_zshrc $tmp/.zshrc
    cp -r $CFG/.zshrc.d $tmp/.zshrc.d
    cp $CFG/.ext.zsh $tmp/
    mkdir -p $tmp/.config
    cp $CFG/_tmux.conf $tmp/.tmux.conf
    mkdir -p $tmp/.config/helix
    cp $CFG/helix/* $tmp/.config/helix
    mkdir -p $tmp/.local/bin
    cp /usr/local/bin/{just,watchexec,yq,rg,fd,dust,btm,xh} $tmp/.local/bin
    #tar hzcvf - --transform "s|^$d\(.*\)|\1|" -C /tmp/cfg $d
    # cp -r $CFG/nvim $tmp/.config/
    tar \
            --exclude='*/.git*' \
            --exclude='*/__pycache__*' \
            --exclude='*/nvim-luapad/gifs*' \
            --exclude='*/plugged/ultisnips/doc/*' \
            --exclude='*/plugged/vimspector/gadgets/*' \
            --exclude='plugged/LeaderF/autoload/leaderf/fuzzyMatch_C/build' \
            -cf - -C $CFG nvim | tar -xf - -C $tmp/.config
    rm -f $tmp/.config/nvim/.netrwhist
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
            -cf - -C $CFG nvim | tar -xf - -C $tmp/
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
    for i in $*
        echo "--------- to $i"
        cmd+="| tee >($sshcmd $i \"rm -rf ~/.config/nvim; tar zxf - --strip-component=1; chown \\\$(id -u):\\\$(id -g) -R ~/{.zshrc,.zshrc.d}\") "
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd

    echo "========= deploy neovim"
    local cmd="cat $HOME/nvim-linux64.tar.gz "
    for i in $*
        echo "--------- to $i"
        cmd+="| tee >($sshcmd $i \"tar zxf - -C /usr/local/ --strip-components=1\")"
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd

    echo "========= deploy helix"
    local cmd="cat $HOME/helix-*-x86_64-linux.tar.xz "
    for i in $*
        echo "--------- to $i"
        cmd+="| tee >($sshcmd $i \"sudo mkdir -p /opt/helix; sudo tar -Jxf - -C /opt/helix --strip-components=1; sudo ln -sf /opt/helix/hx /usr/local/bin\")"
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

export PATH=/home/nash/.tiup/bin:$PATH

export CRICTL=docker
