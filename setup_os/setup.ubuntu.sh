function base {
    ssh $1 "set -eux \
        ; apt update \
        ; apt upgrade \
        ; apt install zsh zstd sqlite3 python3 vim git tmux rsync wget curl jq tree httpie"

}

function vscode {
    scp ~plt/vscode-server.tar.zst $1:~
    ssh $1 "set -eux \
        ; [ ! -d ~/.vscode-server ] && mkdir ~/.vscode-server \
        ; cd ~/.vscode-server \
        ; zstd -d -c ~/vscode-server.tar.zst | tar xvf -"
}

function docker {
    ssh $1 "set -eux \
        ; apt install docker.io \
        ; systemctl enable docker \
        ; systemctl start docker "
}

function ext {
    just_version=0.4.4 watchexec_version=1.10.3 ycat_version=0.2.5 task_version=3.0.0-preview2
    just_url=https://github.com/casey/just/releases/download/v${just_version}/just-v${just_version}-x86_64-unknown-linux-musl.tar.gz
    watchexec_url=https://github.com/watchexec/watchexec/releases/download/${watchexec_version}/watchexec-${watchexec_version}-x86_64-unknown-linux-musl.tar.gz
    ycat_url=https://github.com/alxarch/ycat/releases/download/v${ycat_version}/ycat_${ycat_version}_Linux_x86_64.tar.gz
    ssh $1 "set -eux \
      ; mkdir -p /usr/local/bin \
      ; wget -q -O- ${just_url} \
        | tar zxf - -C /usr/local/bin just \
      ; wget -q -O- ${watchexec_url} \
        | tar zxf - --strip-components=1 -C /usr/local/bin watchexec-${watchexec_version}-x86_64-unknown-linux-musl/watchexec \
      ; wget -q -O- ${ycat_url} \
        | tar zxf - -C /usr/local/bin ycat"

}

function mig {
    dist=migration
    git archive --format=tar --remote=git@g.eng:nash/migrate_191014160127.git master | ssh $1 "set -eux \
        ; [ ! -d $dist ] && mkdir $dist \
        ; tar -xf - -C $dist "
}

function docker-img {
    cat ~/pub/Platform/php/phpf.tar.zst | s $1 'zstd -d | docker load'
}

function rx {
  if ssh $2 "grep -Fxq \"$1\" ~/.setup.ubuntu.manifest"; then
    echo "===> $1 has done!"
  else
    echo "===> $1"
    eval $* && ssh $2 "echo $1 >> ~/.setup.ubuntu.manifest"
  fi
}

() {
    WORK_HOST=xmh
    rx base ${WORK_HOST}
    rx docker ${WORK_HOST}
    rx vscode ${WORK_HOST}
    rx docker-img ${WORK_HOST}
    rx mig ${WORK_HOST}
    rx ext ${WORK_HOST}
    rx deploy-to-server ${WORK_HOST}
}