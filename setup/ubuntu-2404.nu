let apps = [
    {name: git, tag: [dev]}
    {name: ripgrep, tag: [dev]}
    {name: jq, tag: [dev]}
    {name: curl, tag: [network dev]}
    {name: podman, tag: container}
    {name: buildah, tag: container}
    {name: skopeo, tag: container}
    {name: wireguard-tools, tag: [network]}
    {name: tree}
    {name: wget}
    {name: sqlite}
    {name: xclip}
    {name: ibus-rime}
    {name: rime-data-wubi}
]

sudo apt install ...($apps | get name)


print -e 'setup podman'
let registries = '
    unqualified-search-registries = ["docker.io"]
    [[registry]]
    insecure = true
    location = "registry.s"
' | outdent

$registries | str join (char newline) | save -f /etc/containers/registries.conf


print -e 'setup wireguard'
for c in (ls ~/.ssh/wg* | get name) {
    sudo cp $c /etc/wireguard/
    ssc enable --now $"wg-quick@($c | path parse | get stem)"
}


print -e 'sudo without password'
sudo sed -i 's/^.*\(%sudo.*\)ALL$/\1NOPASSWD: ALL/g' /etc/sudoers

# setxkbmap -option 'ctrl:swapcaps'

# /etc/hostname

print -e 'swap ctrl and caps'
sudo sed -i 's/\(XKBOPTIONS\)=""/\1="ctrl:swapcaps"/' /etc/default/keyboard
sudo dpkg-reconfigure keyboard-configuration
