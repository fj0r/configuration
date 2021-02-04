export NVIM_PRESET=full

# docker save xxx | r='docker load'; parallel-ssh a b c

export PATH=/opt/nu:$PATH

if [ -n "$VIMRUNTIME" ]; then
    alias v=drop
else
    alias v='nvim -u $CFG/nvim-coc/init.vim'
fi

alias vv='nvim -u $CFG/nvim/init.vim'
alias vn='nvim -u $CFG/nvim/init.vim'
alias vl='nvim -u $CFG/nvim-lua-example/init.lua'
installNeovim () {
    curl -sSL https://github.com/neovim/neovim/releases/download/${NVIM_VERSION:-nightly}/nvim-linux64.tar.gz \
      |sudo tar zxf - -C /usr/local --strip-components=1
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

export EMAIL_TOKEN=hzowrolckpfeeabb
export EMAIL_SERVER=smtp.qq.com
export EMAIL_ACCOUNT=nash@iffy.me
export EMAIL_RECIPIENTS='
aibric@163.com:王虎
791816330@qq.com:刘斌
nash@iffy.me:我
'


function efav {
    entf nash@iffy.me "⭐️$1" $2
}

hash -d tut="$HOME/pub/Tutorial"
hash -d plt="$HOME/pub/Platform"

source /home/nash/k8s/istio-*/tools/_istioctl

alias y="ipython3"


[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

alias ytd="youtube-dl --proxy http://localhost:1081"
alias ytdm="ytd -x --audio-quality 0 "

function github_version {
    github_header="Accept: application/vnd.github.v3+json"
    github_api=https://api.github.com/repos
    curl -sSL -H $github_header $github_api/${1}/releases | jq -r '.[].tag_name'
}

export PATH=/home/nash/.tiup/bin:$PATH
