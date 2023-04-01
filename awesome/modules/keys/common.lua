local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
local machi = require("layout-machi")
local cyclefocus = require("cyclefocus")
local lain = require("lain")
local client = client
local awesome = awesome

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

local revelation = require("revelation")
revelation.init()

local cycle_filter_legacy = function(c, source_c)
    local st = {}
    for s in screen do
        local t = s.selected_tag
        st[s] = t
    end
    for _, t in ipairs(c:tags()) do
        return st[c.screen] == t
        -- for _, t2 in ipairs(source_c:tags()) do
        --     if t == t2 then
        --         return true
        --     end
        -- end
    end
    return false
end


return function(conf, meta)
    local terminal = conf.terminal or "x-terminal-emulator"

    local quake = lain.util.quake {
        app = terminal,
        settings = function(c) c.sticky = false end,
        height = 0.3
    }

    local alt = 'Mod1'
    local ctrl = 'Control'
    local shift = 'Shift'
    local tab = 'Tab'

    return gears.table.join(
        awful.key({ meta, }, "w", hotkeys_popup.show_help,
            { description = "show help", group = "awesome" }),
        awful.key({ meta, }, "Left", awful.tag.viewprev,
            { description = "view previous", group = "tag" }),
        awful.key({ meta, }, "Right", awful.tag.viewnext,
            { description = "view next", group = "tag" }),
        -- Standard program
        awful.key({ meta, }, "Return", function() awful.spawn(terminal) end,
            { description = "open a terminal", group = "launcher" }),
        awful.key({ meta, ctrl }, "r", awesome.restart,
            { description = "reload awesome", group = "awesome" }),
        awful.key({ meta, shift }, "q", awesome.quit,
            { description = "quit awesome", group = "awesome" }),
        awful.key({ meta, shift }, "/", function() awful.spawn("xscreensaver-command -lock") end,
            { description = "lock screen", group = "awesome" }),
        awful.key({ meta, }, ".", function() machi.default_editor.start_interactive() end,
            { description = "edit the current layout if it is a machi layout", group = "layout" }),
        -- switch
        cyclefocus.key({ alt, }, tab,
            {
                cycle_filters = { cyclefocus.filters.same_screen, cyclefocus.filters.common_tag },
                debug_use_naughty_notify = true,
                keys = { tab, 'ISO_Left_Tab' }
            },
            { description = "cycle focus", group = "client" }),
        awful.key({ meta, }, tab, function() awful.spawn("rofi -show") end,
            { description = "rofi -show window", group = "launcher" }),
        awful.key({ alt, }, '`', function() awful.screen.focus_relative(1) end,
            { description = "focus the next screen", group = "screen" }),
        awful.key({ meta, }, "`", function() awful.spawn("rofi -show run") end,
            { description = "rofi -show run", group = "launcher" }),
        awful.key({ meta, }, "q", function() quake:toggle() end,
            { description = "quake", group = "launcher" }),
        awful.key({ meta, }, "v", function() revelation({ curr_tag_only = true }) end,
            { description = "revelation", group = "client" }),
        -- Prompt
        awful.key({ meta, }, "o", function() awful.spawn("rofi -show run") end,
            { description = "rofi -show run", group = "launcher" }),
        awful.key({ meta }, "x",
            function()
                awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().my_promptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                }
            end,
            { description = "lua execute prompt", group = "awesome" }),


        awful.key({ meta, }, "j",
            function()
                awful.client.focus.byidx(1)
            end,
            { description = "focus next by index", group = "client" }
        ),
        awful.key({ meta, }, "k",
            function()
                awful.client.focus.byidx(-1)
            end,
            { description = "focus previous by index", group = "client" }
        ),
        awful.key({ meta, }, "e", function() my_mainmenu:show() end,
            { description = "show main menu", group = "awesome" }),

        -- Layout manipulation
        awful.key({ meta, shift }, "j", function() awful.client.swap.byidx(1) end,
            { description = "swap with next client by index", group = "client" }),
        awful.key({ meta, shift }, "k", function() awful.client.swap.byidx(-1) end,
            { description = "swap with previous client by index", group = "client" }),
        awful.key({ meta, }, "u", awful.client.urgent.jumpto,
            { description = "jump to urgent client", group = "client" }),

        awful.key({ meta, }, "/", function() machi.switcher.start(client.focus) end,
            { description = "switch between windows for a machi layout", group = "layout" }),
        awful.key({ meta, }, "l", function() awful.tag.incmwfact(0.05) end,
            { description = "increase master width factor", group = "layout" }),
        awful.key({ meta, }, "h", function() awful.tag.incmwfact(-0.05) end,
            { description = "decrease master width factor", group = "layout" }),
        awful.key({ meta, shift }, "h", function() awful.tag.incnmaster(1, nil, true) end,
            { description = "increase the number of master clients", group = "layout" }),
        awful.key({ meta, shift }, "l", function() awful.tag.incnmaster(-1, nil, true) end,
            { description = "decrease the number of master clients", group = "layout" }),
        awful.key({ meta, ctrl }, "h", function() awful.tag.incncol(1, nil, true) end,
            { description = "increase the number of columns", group = "layout" }),
        awful.key({ meta, ctrl }, "l", function() awful.tag.incncol(-1, nil, true) end,
            { description = "decrease the number of columns", group = "layout" }),
        awful.key({ meta, }, "n", function() awful.layout.inc(1) end,
            { description = "select next", group = "layout" }),
        awful.key({ meta, shift }, "n", function() awful.layout.inc(-1) end,
            { description = "select previous", group = "layout" }),


        awful.key({ meta, }, "z", function() awful.spawn("zeal") end,
            { description = "zeal", group = "launcher" }),
        -- gpick
        awful.key({ meta, }, "p", function() awful.spawn("gpick") end,
            { description = "gpick", group = "launcher" }),
        -- screenshot
        awful.key({ meta, }, "s",
            function()
                awful.spawn.with_shell("sleep 0.3 && scrot '%Y%m%d%H%M_$wx$h.png' -s -e 'mv $f ~/Screenshots/'",
                    false)
            end,
            { description = "screenshot", group = "launcher" })

        -- awful.key({ modkey },            "r",     function () awful.screen.focused().my_promptbox:run() end,
        --          {description = "run prompt", group = "launcher"}),
    )
end
