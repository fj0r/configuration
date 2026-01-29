#!/usr/bin/env nu

export def main [app_id] {
    let apps = cos-cli info --json | from json | get apps
    let rules = {
        term: {
            cond: {|x|
                (
                  $x.app_id? | default '' | $in =~ ghostty
                ) and (
                  $x.title? | default '' | str starts-with 'dev'
                )
            }
            cmd: [ghostty -e zellij attach --create dev]
        }
        browser: {
            cond: {|x|
                $x.app_id? | default '' | $in =~ vivaldi
            }
            cmd: [vivaldi]
        }
        chat: {
            cond: {|x|
                $x.app_id? | default '' | $in =~ wechat
            }
            cmd: [wechat]
        }
    }
    if $app_id in $rules {
        let cond = $rules | get $app_id | get cond
        let a = $apps | where $cond
        if ($a | is-empty) {
            let p = $rules | get $app_id | get cmd
            ^($p | first) ...($p | skip 1)
        } else {
            cos-cli activate -i $a.0.index
        }
    }
}
