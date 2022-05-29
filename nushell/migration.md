## migrate

- config struct
- prompt
    - [ ] k8s
    - [ ] startship
- alias 
    - [ ] utils
        - [ ] a: alias
        - [ ] e: nvim
        - [ ] t: tmux
        - alias sget='wget -m -k -E -p -np -e robots=off'
        - toggle-proxy
        - slam
        - wg
        - if (( $+commands[ss] )); then
              alias ns='ss -tulwnp'
          else
              alias ns="netstat -plnetu"
          fi
    - [ ] docker
    - [ ] docker dev helper
    - [ ] k8s
    - [ ] ssh
    - [ ] git
- completions
    - [ ] podman
    - [ ] k8s
    - [ ] ssh
    - [ ] git
- edit
    - [.] `enter` for `ls`
    - [.] `tab` in empty for cd: direct path and then `enter`
- named dir: history?
- history
- alternatives
    - [x] yq, jq, rq ...
        - [ ] modify
    - [x] fd (recursive)
        - ls **/*
    - [ ] sd
    - [ ] rg?
    - [ ] just
    - [ ] watchexec
    - [ ] btm
    - [ ] dog
    - [ ] dust
    - [ ] curl, xh
    - [ ] base64
