### {{{ base.nu
$env.comma_scope = {|_|{ created: '2024-07-06{6}09:51:58' }}
$env.comma = {|_|{}}
### }}}

### {{{ 01_env.nu
for e in [nuon toml yaml json] {
    if ($".env.($e)" |  path exists) {
        open $".env.($e)" | load-env
    }
}
### }}}

'ssh_args'
| comma val null [
    -o StrictHostKeyChecking=no
    -o UserKnownHostsFile=/dev/null
    -p 7788
    nixos@localhost
]

'sync'
| comma fun {|a,s|
    cat configuration.nix
    | ^ssh ...[
        ...$s.ssh_args
        'sudo tee /mnt/etc/nixos/configuration.nix'
    ]
}

'ssh'
| comma fun {|a,s|
    ^ssh ...$s.ssh_args
}

'wstunnel'
| comma fun {|a,s|
    for i in [
        'passwd'
        'curl -H "Host: file.s" http://172.178.5.123/wstunnel -O'
        'chmod +x wstunnel'
        './wstunnel client -R tcp://7788:localhost:22 ws://10.0.2.2:7787'
    ] { print $"(ansi grey)($i)(ansi reset)" }
    wstunnel server ws://0.0.0.0:7787
}

