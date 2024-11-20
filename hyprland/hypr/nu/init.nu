export def --env hypr-state-init [] {
    init-db HYPRLAND_STATE ([$nu.cache-dir 'hyprland.db'] | path join) {|sqlx, Q|
        for s in [
            "CREATE TABLE IF NOT EXISTS workspaces (
                id INT PRIMARY KEY,
                name TEXT,
                created INT,
                updated INT,
                current INT DEFAULT 0
            );"
        ] {
            do $sqlx $s
        }
    }
}

def --env init-db [env_name:string, file:string, hook: closure] {
    let begin = date now
    if $env_name not-in $env {
        {$env_name: $file} | load-env
    }
    if ($file | path exists) { return }
    {_: '.'} | into sqlite -t _ $file
    open $file | query db "DROP TABLE _;"
    do $hook {|s| open $file | query db $s } {|...t| Q ...$t }
    print $"(ansi grey)created database: $env.($env_name), takes ((date now) - $begin)(ansi reset)"
}
