export def main [] {
    let ws = hyprctl workspaces
    | lines
    | filter { $in | str starts-with 'workspace' }
    | length
    let cur = hyprctl activeworkspace
    | lines
    | first
    | parse -r 'workspace ID (?<id>[0-9]+)'
    | get id.0
    | into int
    hyprctl dispatch workspace (($cur + 1) mod $ws)
}
