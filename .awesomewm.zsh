function awm {
    awesome-client "naughty = require('naughty'); naughty.notify {title='$1', text='${2:-CLI Notification}', position='top_middle', timeout=0}"
}

function awr {
    local msg
    local err
    local pos=$(awesome-client 'awful=require"awful";return awful.screen.focused({client=true}).index..": "..awful.tag.selected().index' | awk '{print $2,$3}')
    if ! msg="$(eval $* 2>&1)" ; then
        err=1
    fi
    local head='naughty=require"naughty";naughty.notify '
    local cmd="${*//$'\n'/ }"
    if [ -z $err ] ; then
        awesome-client "$head{title='${pos//\"/} SUCCESS', text='$cmd', position='top_middle', timeout=0}"
    else
        awesome-client "$head{title='${pos//\"/} FAILED', text='$cmd', position='bottom_middle', timeout=0}"
    fi
}