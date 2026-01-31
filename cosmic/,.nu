export def set-keys [--test] {
    let t = '
    (
        modifiers: [ ],
        key: "F{n}",
        description: Some("F{n}"),
    ): Spawn("nu /home/master/Configuration/cosmic/focus.nu {n}"),
    '
    | str trim --char "\n"
    | str trim -r
    mut r = []
    for i in 1..12 {
        $r ++= [({n: $i} | format pattern $t)]
    }
    let c = '
    {{
    {fn}
        (
            modifiers: [
                Super,
                Ctrl,
            ],
            key: "s",
        ): ToggleSticky,
        (
            modifiers: [
                Super,
            ],
            key: "q",
        ): Spawn("flameshot gui"),
    }}
    '
    | str trim --char "\n"
    | str replace -rma '^ {4}' ''
    let c = $r | str join (char newline) | {fn: $in} | format pattern $c
    if $test { return $c }
    const s = path self custom
    $c | save -f $s
    [y n]
    | input list 'apply?'
    | if $in == y {
        $c | save -f ($env.HOME | path join .config/cosmic/com.system76.CosmicSettings.Shortcuts/v1/custom)
    }
}
