let nu-config-dir = ($nu.config-path | path dirname)
ls $'($nu-config-dir)/*.nu' |
    where ($it.name | path parse | get stem) not-in ['__init', '__bootstrap'] |
    get name |
    each { echo $'source ($it)(char newline)' } |
    save $'($nu-config-dir)/__init.nu'

source __init.nu
