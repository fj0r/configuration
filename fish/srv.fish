function addsrvo
    sudo adduser --system --group --no-create-home $argv[1]
end

function cpsystemdsrv
    sudo cp $argv[1] /etc/systemd/system/
end

function deploy-to-server
    set cfg ~/pub/Configuration/
    for i in $argv
        rsync -avz -e ssh $cfg/fish/ {$i}:~/.config/fish
        rsync -avz -e ssh $cfg/_vimrc {$i}:~/.vimrc
        rsync -avz -e ssh $cfg/_vim/ {$i}:~/.vim
        rsync -avz -e ssh $cfg/_config/fish/ {$i}:~/.config/fish
    end
end