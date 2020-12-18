if [ -x "$(command -v yay)" ]; then
    pkgi='yay -S'
elif [ -x "$(command -v pacman)" ]; then
    pkgi='sudo pacman -S'
elif [ -x "$(command -v apt-get)" ]; then
    pkgi='sudo apt-get install -y'
fi

function install {
    l=()
    for i in $@; do
        if ! [ -x "$(command -v $i)" ]; then
            l+=($i)
        fi
    done
    echo $pkgi ${l[@]}
    $pkgi ${l[@]}
}

install bat exa fd rg ncdu


