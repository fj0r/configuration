local awful = require("awful")
local M = {
    autorun = {
        "setxkbmap -option 'ctrl:swapcaps'",
        --"source ~/.config/xrandr.rc",
    },
    autorun_once = {
        "gnome-session --systemd-service",
        -- "/usr/lib/polkit-1/polkitd",
        "xscreensaver -no-splash",
        "ibus-daemon --xim --replace --daemonize",
    },
    sidebar = "right",
    monitor = {
        partitions = { "/", "/home" }
    },
    layouts = {},

    rules = {
        {
            rule = { class = "Vivaldi" },
            screen = awful.screen.focused,
            tags = { "1", "2", "10", "X" }
        },
    },
    tags = {
        { name = '1', layout = 'tile' },
        { name = '2', layout = 'tile' },
        { name = '3', layout = 'centerwork',
            apps = {
                { class = "Vivaldi", floating = true, screen = 2 },
                { class = "qutebrowser", floating = true },
            }
        },
        { name = '4', layout = 'centerwork' },
        { name = '5', layout = 'tile.bottom' },
        { name = '6', layout = 'tile.bottom' },
        { name = '7', layout = 'tile.bottom' },
        { name = '8', layout = 'centerwork' },
        { name = '9' },
        { name = 'X' },
    },
    theme = {
        gap = 0,
        urgent_color = 'dde175',
        border = {
            width = 1,
            focus = '6a6e09',
            -- focus: 'ffd8b1',
            -- focus: '82a67d',
        },
        wallpaper = os.getenv('HOME') .. '/Pictures/wallpaper',
        powerline_taglist = false
    },
    editor = 'wezterm --config-file ' .. os.getenv('HOME') .. '/Configuration/wezterm/nvim.lua',
    terminal = 'wezterm',
    -- inspect with xprop
    floating = {
        instance = {
            "DTA", "copyq", "pinentry"
        },
        class = {
            'Zeal',
            'VirtualBox Machine',
            'Arandr',
            'Blueman-manager',
            'Gpick',
            'Kruler',
            'MessageWin',
            'Sxiv',
            'Tor Browser',
            'Wpa_gui',
            'veromix',
            'xtightvncviewer',
        },
        name = {
            'Event Tester',
            'qutebrowser-editor',
        },
        role = {
            'AlarmWindow',
            'ConfigManager',
            'pop-up',
        }
    }
}

-- require("utils").say(M.rules)

return M
