typeset -A progs
local progs=(
    cat bat
    #du ncdu
    ls exa
    find fd
    fz fzf
    #'grep' rg
)

local k v
for k v in ${(kv)progs}
do
    (( $+commands[$v] )) && alias $k=$v
done
