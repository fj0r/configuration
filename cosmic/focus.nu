#!/usr/bin/env nu

const RULE = path self apps.yaml

def flt [rules] {
    let n = $in
    $rules | reduce -f true {|i,a|
        let r = $i | split row -r '\s+'
        $a and match $r.1 {
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
