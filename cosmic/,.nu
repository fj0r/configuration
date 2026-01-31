export def set-keys [--test] {
    const ks = path self apps.yaml
    let ks = open $ks | get keybindings
    let c = 1..12
    | each {|x|
        gen-keybinding {
            modifiers: []
            key: F($x)
            action: $"Spawn\(\"nu ($env.HOME)/Configuration/cosmic/focus.nu ($x)\"\)"
        }
    }
    | append ($ks | each { gen-keybinding $in })
    | str join (char newline)
    | $"{\n($in)\n}"
    if $test { return $c }
    [y n]
    | input list 'apply?'
    | if $in == y {
        $c | save -f ($env.HOME | path join .config/cosmic/com.system76.CosmicSettings.Shortcuts/v1/custom)
    } else {
        $c
    }
}

def gen-keybinding [d] {
    let t = '    (
        modifiers: [
            {ms}
        ],
        key: "{key}",
    ): {action},'
    $d
    | insert ms ($d.modifiers | each { $"($in)," } | str join "\n            ")
    | format pattern $t
}
