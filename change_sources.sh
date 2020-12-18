case $(grep ^ID= /etc/os-release | sed 's/ID=\(.*\)/\1/') in
  debian | ubuntu )
    cp /etc/apt/sources.list /etc/apt/sources.list.$(date +%y%m%d%H%M%S)
    sed -i 's/\(.*\)\(security\|deb\).debian.org\(.*\)main/\1ftp2.cn.debian.org\3main contrib non-free/g' /etc/apt/sources.list
  ;;
  alpine )
    cp /etc/apk/repositories /etc/apk/repositories.$(date +%y%m%d%H%M%S)
    sed -i 's/dl-cdn.alpinelinux.org/mirror.tuna.tsinghua.edu.cn/g' /etc/apk/repositories
  ;;
  * )
esac