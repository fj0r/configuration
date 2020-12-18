set -U fish_greeting

set FISH_CFG_PATH $HOME/.config/fish

for f in (ls $FISH_CFG_PATH/*.fish | grep -v '.*/config.fish')
    source $f
end

for f in (ls $FISH_CFG_PATH/functions/ | grep -v '^fish_')
    source $FISH_CFG_PATH/functions/$f
end
