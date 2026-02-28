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
        let r = if $f { $r | skip 1 } else { $r }
        let b = match $r.1 {
            '==' => {
                ($n | get $r.0) == $r.2
            }
            '!=' => {
                ($n | get $r.0) != $r.2
            }
            '=~' => {
                ($n | get $r.0) =~ $r.2
            }
            'starts-with' => {
                ($n | get $r.0) | str starts-with $r.2
            }
        }
        let b = if $f { not $b } else { $b }
        $a and $b
    }
}

def to-list [v] {
    if ($v | describe -d).type == list {
        $v
    } else {
        [$v]
    }
}

export def main [key] {
    let rules = open $RULE | get apps.rules
    let r = $rules
    | where {|x| ($key | into int) in (to-list $x.keys) }
    | get -o 0
    if ($r | is-not-empty) {
        let a = list | where { $in | flt $r.filter }
        if ($a | is-empty) {
            let p = $r | get cmd
            run-external ($p | first) ...($p | skip 1)
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
