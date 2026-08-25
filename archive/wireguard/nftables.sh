nft add table wg nat
nft add chain wg nat PREROUTING { type nat hook prerouting priority 0 \; }
nft add chain wg nat POSTROUTING { type nat hook postrouting priority 100 \; }
nft add rule wg nat POSTROUTING ip saddr 10.0.0.0/24 oifname "eth0" masquerade
