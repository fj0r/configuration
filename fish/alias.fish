alias mv    'mv -i'
alias rm    'rm -i'
alias ll    'ls -l'
alias mdp   'mkdir -p'
alias grep  'grep --color=auto'
alias diff  'diff -u'
alias v     'vim'
alias g     'git'

alias n     'npm'
alias nx    'npm run'
alias nb    'npm run build'
alias ni    'npm install'
alias nx    'npm run dev'
alias ns    'npm start'

alias c     'cargo'
alias ci    'cargo install'
alias cx    'cargo run'
alias cb    'cargo build'

alias s     'stack'
alias si    'stack install'
alias sx    'stack exec'
alias sr    'stack repl'

alias ssc   'systemctl'

alias pc    'proxychains4 -q'
alias sget  'wget -c -r -np -k -L -p'

function runmd
    for i in $argv
        awk '/```/{f=0} f; /```bash/{f=1}' {$i} | /bin/bash -ex
    end
end

function findBigFiles
    find . -type f -size +$argv[1] -print0 | xargs -0 du -h | sort -nr
end