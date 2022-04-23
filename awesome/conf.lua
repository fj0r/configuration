local M = {
    autorun = {
        "setxkbmap -option 'ctrl:swapcaps'",
        "source ~/.config/xrandr.rc",
    },
    autorun_once = {
        "gnome-session --systemd-service" ,
        -- "/usr/lib/polkit-1/polkitd",
        -- "xscreensaver -no-splash",
        "ibus-daemon --xim --replace --daemonize",
    },
    sidebar = "right",
    monitor = {
        partitions = { "/", "/home" }
    },
    tags = {
        { name = '1', layout = 'tile' },
        { name = '2', layout = 'tile' },
        { name = '3', layout = 'centerwork'
        , apps = { { class = "Vivaldi", floating = true, screen = 2 }
                 , { class = "Chromium", floating = true },
                 }
        },
        { name = '4', layout = 'centerwork' },
        { name = '5', layout = 'tile.bottom'  },
        { name = '6', layout = 'tile.bottom'  },
        { name = '7', layout = 'tile.bottom'  },
        { name = '8', layout = 'centerwork' },
        { name = '9' },
        { name = 'X' },
    },
    theme = {
        gap = 0,
        urgent_color = 'dde175',
        border = {
            width = 2,
            focus = '6a6e09',
            -- focus: 'ffd8b1',
            -- focus: '82a67d',
        },
        wallpaper = '/wallpapers',
        powerline_taglist = false
    },
    editor = 'nvim',
    terminal = 'alacritty',
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

local awful = require("awful")
local get = require('utils.get')

local tags = {}
M.layouts = {}
M.rules = {}
for k, v in ipairs(M.tags) do
    table.insert(tags, v.name)
    table.insert(M.layouts, v.layout and #v.layout ~= 0
            and get(awful.layout.suit, v.layout)
            or awful.layout.suit.tile)
    for _, term in ipairs(v.apps or {}) do
        local rule = {}
        local count = 0
        for p in ipairs({'instance', 'class', 'name', 'role'}) do
            if term[p] then
                rule[p] = term[p]
                term[p] = nil
                count = count + 1
            end
        end
        if count > 0 then
            term.screen = term.screen or 1
            term.tag = k
            table.insert(M.rules, {
                rule = rule,
                properties = term
            })
        end
    end
end

M.tags = tags


return M
