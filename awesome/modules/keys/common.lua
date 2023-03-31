local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
local machi = require("layout-machi")
local cyclefocus = require("cyclefocus")
local lain = require("lain")

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


return function(conf, modkey)
    local terminal = conf.terminal or "x-terminal-emulator"

    local _quake = lain.util.quake {
        app = terminal,
        settings = function(c) c.sticky = false end,
        height = 0.382
    }

    return gears.table.join(
        awful.key({ modkey, }, "w", hotkeys_popup.show_help,
            { description = "show help", group = "awesome" }),
        awful.key({ modkey, }, "Left", awful.tag.viewprev,
            { description = "view previous", group = "tag" }),
        awful.key({ modkey, }, "Right", awful.tag.viewnext,
            { description = "view next", group = "tag" }),

        awful.key({ modkey, }, "j",
            function()
                awful.client.focus.byidx(1)
            end,
            { description = "focus next by index", group = "client" }
        ),
        awful.key({ modkey, }, "k",
            function()
                awful.client.focus.byidx(-1)
            end,
            { description = "focus previous by index", group = "client" }
        ),
        awful.key({ modkey, }, "e", function() my_mainmenu:show() end,
            { description = "show main menu", group = "awesome" }),

        -- Layout manipulation
        awful.key({ modkey, "Shift" }, "j", function() awful.client.swap.byidx(1) end,
            { description = "swap with next client by index", group = "client" }),
        awful.key({ modkey, "Shift" }, "k", function() awful.client.swap.byidx(-1) end,
            { description = "swap with previous client by index", group = "client" }),
        awful.key({ modkey, }, "u", awful.client.urgent.jumpto,
            { description = "jump to urgent client", group = "client" }),
        awful.key({ modkey, }, "Tab", function() awful.screen.focus_relative(1) end,
            { description = "focus the next screen", group = "screen" }),
        awful.key({ "Mod1", }, "`", function() revelation({ curr_tag_only = true }) end,
            { description = "revelation", group = "client" }),
        cyclefocus.key({ "Mod1", }, "Tab", {
            cycle_filters = { cyclefocus.filters.same_screen, cyclefocus.filters.common_tag },
            debug_use_naughty_notify = true,
            keys = { 'Tab', 'ISO_Left_Tab' }
        },
            { description = "cycle focus", group = "client" }),

        -- Standard program
        awful.key({ modkey, }, "Return", function() awful.spawn(terminal) end,
            { description = "open a terminal", group = "launcher" }),
        awful.key({ modkey, "Control" }, "r", awesome.restart,
            { description = "reload awesome", group = "awesome" }),
        awful.key({ modkey, "Shift" }, "q", awesome.quit,
            { description = "quit awesome", group = "awesome" }),
        awful.key({ modkey, "Shift" }, "/", function() awful.spawn("xscreensaver-command -lock") end,
            { description = "lock screen", group = "awesome" }),
        awful.key({ modkey, }, ".", function() machi.default_editor.start_interactive() end,
            { description = "edit the current layout if it is a machi layout", group = "layout" }),
        awful.key({ modkey, }, "/", function() machi.switcher.start(client.focus) end,
            { description = "switch between windows for a machi layout", group = "layout" }),
        awful.key({ modkey, }, "l", function() awful.tag.incmwfact(0.05) end,
            { description = "increase master width factor", group = "layout" }),
        awful.key({ modkey, }, "h", function() awful.tag.incmwfact(-0.05) end,
            { description = "decrease master width factor", group = "layout" }),
        awful.key({ modkey, "Shift" }, "h", function() awful.tag.incnmaster(1, nil, true) end,
            { description = "increase the number of master clients", group = "layout" }),
        awful.key({ modkey, "Shift" }, "l", function() awful.tag.incnmaster(-1, nil, true) end,
            { description = "decrease the number of master clients", group = "layout" }),
        awful.key({ modkey, "Control" }, "h", function() awful.tag.incncol(1, nil, true) end,
            { description = "increase the number of columns", group = "layout" }),
        awful.key({ modkey, "Control" }, "l", function() awful.tag.incncol(-1, nil, true) end,
            { description = "decrease the number of columns", group = "layout" }),
        awful.key({ modkey, }, "n", function() awful.layout.inc(1) end,
            { description = "select next", group = "layout" }),
        awful.key({ modkey, "Shift" }, "n", function() awful.layout.inc(-1) end,
            { description = "select previous", group = "layout" }),

        -- Prompt
        awful.key({ modkey, }, "i", function() awful.spawn("rofi -show") end,
            { description = "rofi -show window", group = "launcher" }),
        awful.key({ modkey, }, "o", function() awful.spawn("rofi -show run") end,
            { description = "rofi -show run", group = "launcher" }),
        awful.key({ modkey, }, "z", function() awful.spawn("zeal") end,
            { description = "zeal", group = "launcher" }),

        -- screenshot
        awful.key({ modkey, }, "s",
            function()
                awful.spawn.with_shell("sleep 0.3 && scrot '%Y%m%d%H%M_$wx$h.png' -s -e 'mv $f ~/Screenshots/'",
                    false)
            end,
            { description = "截图", group = "launcher" }),

        -- awful.key({ modkey },            "r",     function () awful.screen.focused().my_promptbox:run() end,
        --          {description = "run prompt", group = "launcher"}),

        awful.key({ modkey }, "x",
            function()
                awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().my_promptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                }
            end,
            { description = "lua execute prompt", group = "awesome" })
    )
end
