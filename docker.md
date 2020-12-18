### 宿主机 ssh
```bash
RUN set -ex && up=`ip route | awk 'NR==1 {print $3}'` \
  ; mkdir ~/.ssh && chmod 700 ~/.ssh && IdFile=up \
  ; echo "-----BEGIN EC PRIVATE KEY-----"                                   >> ~/.ssh/$IdFile \
  ; echo "MHcCAQEEIFEO/DDjCFwSr8ncPLFGHRZHL17n7vAQ8uk2NkBcbFpqoAoGCCqGSM49" >> ~/.ssh/$IdFile \
  ; echo "AwEHoUQDQgAEWRJPj2ctabqDkkFqFwEGGJVojCr1DYR+Rd+8TNok1n2/PFZe/T77" >> ~/.ssh/$IdFile \
  ; echo "D26BKkpqzrehWc0MwrecBru80r89e+9SyA=="                             >> ~/.ssh/$IdFile \
  ; echo "-----END EC PRIVATE KEY-----"                                     >> ~/.ssh/$IdFile \
  ; chmod 600 ~/.ssh/$IdFile \
  ; echo "StrictHostKeyChecking no"         >> ~/.ssh/config \
  ; echo "UserKnownHostsFile /dev/null"     >> ~/.ssh/config \
  ; echo "LogLevel=quiet"                   >> ~/.ssh/config \
  ; echo "Host up"                          >> ~/.ssh/config \
  ; echo "    HostName $up"                 >> ~/.ssh/config \
  ; echo "    User murphy"                  >> ~/.ssh/config \
  ; echo "    IdentityFile ~/.ssh/$IdFile"  >> ~/.ssh/config \
  ; ...
```

```
# 宿主 authorized_keys
ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFkST49nLWm6g5JBahcBBhiVaIwq9Q2EfkXfvEzaJNZ9vzxWXv0++w9ugSpKas63oVnNDMK3nAa7vNK/PXvvUsg= root@93ab5b3b6af6
```

```bash
echo "echo \"\`ip route | awk 'NR==1 {print \$3}'\` up\" >> /etc/hosts"
```


### shell 参数
```bash
set -ex
    -e # 如果命令带非零值返回,立即退出
    -x # 在每个简单命令被扩展之后,显示PS4扩展值,之后是要执行的命令
    -u # 当执行参数括展时,把非设置变量作为错误处理(如果扩展企图出现在非设置变量中,shell显示错误信息.如果不是交互式,则带非零值退出)
```

### user
```bash
ENV NB_USER lab
ENV HOME=/home/${NB_USER}           \
    NB_UID=1000                     \
    LANG=zh_CN.UTF-8
WORKDIR ${HOME}

RUN adduser --disabled-password     \
        --gecos "Default user"      \
        --uid ${NB_UID}             \
        ${NB_USER}                  \
  ; chown -R ${NB_UID} ${HOME}
```

### apk
```bash
RUN set -ex                                                                             \
  ; sed -i 's/dl-cdn.alpinelinux.org/mirrors.ustc.edu.cn/g' /etc/apk/repositories       \
  ; apk add --no-cache --virtual .fetch-deps                                            \
		  ca-certificates openssl tar                                                       \
  ; apk add --no-cache --virtual .build-deps                                            \
      cmake python                                                                      \
  ; ...                                                                                 \
  ; apk del .fetch-deps .build-deps
```
### apt
```bash
RUN set -ex                                                                             \
  ; DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends           \
  ;   ...                                                                               \
  ; apt-get clean                                                                       \
  ; rm -rf /var/lib/apt/lists/*
```
### pip
```bash
mkdir ~/.pip && ph=pypi.mirrors.ustc.edu.cn && echo "[global]\nindex-url = http://$ph/simple\n[install]\ntrusted-host=$ph" > ~/.pip/pip.conf
pip --no-cache-dir install foo
```

### npm
```bash
npm install foo
npm cache clean -f
```




错误
`error creating new backup file '/var/lib/dpkg/status-old': Invalid cross-device link`

```bash
echo N | sudo tee /sys/module/overlay/parameters/metacopy
```


