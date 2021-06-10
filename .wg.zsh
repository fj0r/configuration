alias wq='wg-quick'
alias wqu='wg-quick up'
alias wqd='wg-quick down'

function wqe {
    sudo $EDITOR /etc/wireguard/${1:-wg0}.conf
}

function wqr {
    sudo bash -c "wg syncconf ${1:-wg0} <(wg-quick strip ${1:-wg0})"
}
