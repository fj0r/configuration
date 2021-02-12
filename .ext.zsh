export NVIM_PRESET=core

if [[ ! "$PATH" == */opt/bin* && -d /opt/bin ]]; then
    export PATH=/opt/bin:$PATH
fi

# [ -d $CFG/.fzf ] && source $CFG/.fzf/init.zsh

[ -f $HOME/.user.zsh ] && source $HOME/.user.zsh
[ -f $HOME/.test.zsh ] && source $HOME/.test.zsh
[ -f $HOME/.mutagen-alias ] && source $HOME/.mutagen-alias

if (( $+commands[octosql])); then
    function q {
        export OCTOSQL_CONFIG=$(find . -name '*octo*y*ml')
        octosql $@
    }
fi

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
    mkdir -p $tmp/.local/bin
    cp /usr/local/bin/{just,watchexec,yq,rg} $tmp/.local/bin
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

function deploy-to-server {
    #rsync -av -e ssh $rc $1:~/.zshrc
    #rsync -av --delete -e ssh $CFG/.zshrc.d/ $1:~/.zshrc.d
    #rsync -av -e ssh $CFG/.ext.zsh $1:~/.ext.zsh
    #rsync -av -e ssh $CFG/.kubectl.zsh $1:~/.kubectl.zsh
    #rsync -av -e ssh $CFG/_tmux.conf $1:~/.tmux.conf
    #rsync -av --delete -e ssh $CFG/.fzf/ $1:~/.fzf
    local cmd="cat $HOME/pub/cfg-coc.tgz "
    local sshcmd="ssh"
    for i in $*
        cmd+="| tee >($sshcmd $i \"tar zxf - --strip-component=1; chown \\\$(id -u):\\\$(id -g) -R ~/{.zshrc,.zshrc.d}\") "
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd

    local cmd="cat $HOME/nvim-linux64.tar.gz "
    for i in $*
        cmd+="| tee >($sshcmd $i \"tar -zxf - -C \\\$HOME/.local --strip-components=1\")"
    cmd+="> /dev/null"
    echo $cmd
    eval $cmd


    local cmd="cat $HOME/node-v*-linux-x64.tar.xz"
    for i in $*
        cmd+="| tee >($sshcmd $i \"tar -Jxf - -C \\\$HOME/.local --strip-components=1\")"
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
