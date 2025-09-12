export def main [act args?] {
    match $act {
        cycle => {
            let lst = sqlx $"select id, current from workspaces order by updated desc"
            mut cur = 0
            for i in ($lst | enumerate) {
                if $i.item.current == 1 {
                    $cur = $i.index
                    break
                }
            }
            let ix = $cur + 1
            let ix = if $ix == ($lst | length) { 0 } else { $ix }
            let id = $lst | get $ix | get id
            sqlx $"update workspaces set current = case when id = ($id) then 1 else 0 end"
            hyprctl dispatch workspace $id
        }
        pop => {
            let now = date now | into int
            let all = hyprctl workspaces
            | lines
            | where { $in | str starts-with 'workspace' }
            | parse -r 'workspace ID (?<id>[0-9]+)'
            | get id
            | str join ','
            sqlx $"delete from workspaces where id not in \(($all)\)"
            let id = hyprctl activeworkspace
            | lines
            | first
            | parse -r 'workspace ID (?<id>[0-9]+)'
            | get id.0
            | into int
            sqlx $"update workspaces set updated = ($now) where id = ($id)"
        }
        add => {
            let a = $args | split row ','
            let id = $a.0
            let name = $a.1
            let now = date now | into int
            sqlx $"insert into workspaces \(id, name, created, updated\) values \(
                ($id), (Q $name), ($now), ($now)
            \) on conflict \(id\) do update
            set name = EXCLUDED.name, created = EXCLUDED.created, updated = EXCLUDED.updated"
        }
        delete => {
            let id = $args | split row ',' | first
            sqlx $"delete from workspaces where id = ($id)"
        }
    }
}

def sqlx [s] {
    open ($env.HYPRLAND_STATE | path expand) | query db $s
}

def Q [...t --sep:string=''] {
    let s = $t | str join $sep | str replace -a "'" "''"
    $"'($s)'"
}
