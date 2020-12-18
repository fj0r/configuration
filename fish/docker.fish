alias dk    "docker"
alias di    "docker images"
alias drmi  "docker rmi"
alias dp    "docker ps"
alias dpa   "docker ps -a"
alias dl    "docker logs -ft"
alias dpl   "docker pull"
alias dr    "docker run --rm -tid"
alias drr   "docker run --rm -v (pwd):/world"
alias dri   "docker run --rm -ti -v (pwd):/world"
alias dcs   "docker container stop"
alias dsp   "docker system prune -f"
alias dvi   "docker volume inspect"
alias dvr   "docker volume rm"
alias dvp   "docker volume prune"
alias dvl   "docker volume ls"
alias dvc   "docker volume create"
alias dsv   "docker save"
alias dld   "docker load"
alias dcp   "docker-compose up"
alias dcu   "docker-compose up -d"
alias dcr   "docker-compose restart"
alias dcd   "docker-compose down"

function da
    set f "[ -e /usr/bin/fish ] && fish"
    set z "[ -e /bin/zsh ] && /bin/zsh"
    set b "[ -e /bin/bash ] && /bin/bash"
    docker exec -it $argv[1] /bin/sh -c " $f || $z || $b || /bin/sh"
end

function dvbk
    for i in $argv
        docker run --rm -it -v (pwd):/backup                    \
                            -v {$i}:/data                       \
                            alpine                              \
                                tar zcvf /backup/{$i}_`date +%Y%m%d%H%M%S`.tar.gz -C /data .
    end
end

function dvrs
    docker volume create $argv[2]
    docker run --rm -it -v (pwd):/backup                        \
                        -v $argv[2]:/data                       \
                        alpine                                  \
                            tar zxvf /backup/$argv[1] -C /data
end

function dvis
    docker run -it --rm -v $argv:/data -w /data alpine
end
