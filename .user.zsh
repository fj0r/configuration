export NVIM_PRESET=full

# docker save xxx | r='docker load'; parallel-ssh a b c

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

export EMAIL_TOKEN=knxyhovsyhywdibb

export EMAIL_ACCOUNT=nash@iffy.me
export EMAIL_RECIPIENTS='
aibric@163.com:王虎
yonghengjile@qq.com:王德胜
chenweicn@foxmail.com:陈伟
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
