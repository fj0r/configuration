#!/usr/bin/env nu

const RULE = path self apps.yaml

def flt [rules] {
    let n = $in
    $rules | reduce -f true {|i,a|
        let r = $i | split row -r '\s+'
        let f = if $r.0 == 'not' {
            true
        } else {
            false
        }
        let x = if $f { $r.1 } else { $r.0 }
        let p = if $f { $r.2 } else { $r.1 }
        let y = if $f { $r.3 } else { $r.2 }
        let b = match $p {
            '==' => {
                ($n | get $x) == $y
            }
            '!=' => {
                ($n | get $x) != $y
            }
            '=~' => {
                ($n | get $x) =~ $y
            }
            'starts-with' => {
                ($n | get $x) | str starts-with $y
            }
        }
        if $f {
            $a and (not $b)
        } else {
            $a and $b
        }
    }
}

def to-list [id] {
    if ($id | describe -d).type == list {
        $id
    } else {
        [$id]
    }
}

export def main [app_id] {
    let rules = open $RULE | get apps.rules
    let r = $rules | where {|x| ($app_id | into int) in (to-list $x.id) } | get -o 0
    if ($r | is-not-empty) {
        let a = list | where { $in | flt $r.filter }
        if ($a | is-empty) {
            let p = $r | get cmd
            ^($p | first) ...($p | skip 1)
        } else {
            activate $a.0.index
        }
    }
}

### https://github.com/estin/cos-cli

def list [] {
    cos-cli info --json | from json | get apps
}

def activate [id] {
    cos-cli activate -i $id
}
