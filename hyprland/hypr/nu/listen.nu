# socat -U - UNIX-CONNECT:$XDG_RUNTIME_DIR/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock | while read -r line; do handle "$line"; done

socat -U - $"UNIX-CONNECT:($env.XDG_RUNTIME_DIR)/hypr/($env.HYPRLAND_INSTANCE_SIGNATURE)/.socket2.sock"
| lines
| each {|x|
    let x = $x | split row '>>'
    match $x.0 {
        createworkspacev2 => {
            nu ~/.config/hypr/nu/cycle-workspace.nu add $x.1
        }
        destoryworkspacev2 => {
            nu ~/.config/hypr/nu/cycle-workspace.nu delete $x.1
        }
        _ => {
            # $"\n($ev)::($argv)" | save -a ~/.cache/hypr-event
        }
    }
}
