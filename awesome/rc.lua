-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")
local utils = require 'utils'
local say = utils.say
--local path = utils.script_path() .. 'conf.yml'
--local conf = utils.conf.gen_config(path)
local conf = require('conf')
local modules = require 'modules'

-- Standard awesome library
local gears = require("gears")
local beautiful = require("beautiful")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
-- Theme handling library
-- Notification library
local naughty = require("naughty")
local machi = require("layout-machi")
local cyclefocus = require("cyclefocus")
local hotkeys_popup = require("awful.hotkeys_popup")
local lain = require("lain")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

modules.autorun {
    autorun = conf.autorun,
    autorun_once = conf.autorun_once
}

modules.handle_error()
modules.theme(conf)
local menu = modules.menu(conf)
local set_wallpaper = modules.set_wallpaper(conf)
modules.signal()

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
local modkey = "Mod4"

beautiful.layout_machi = machi.get_icon()
-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    utils.centerwork,
    -- awful.layout.suit.tile.left,
    -- awful.layout.suit.floating,
    awful.layout.suit.tile.bottom,
    -- awful.layout.suit.magnifier,
    -- awful.layout.suit.tile.top,
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
    machi.default_layout,
}
-- }}}



-- {{{ Wibar


modules.screen(conf, menu, set_wallpaper)
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)

-- {{{ Mouse bindings
root.buttons(gears.table.join(
    awful.button({}, 3, function() my_mainmenu:toggle() end),
    awful.button({}, 4, awful.tag.viewnext),
    awful.button({}, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
local quake = lain.util.quake { app = conf.quake or 'alacritty', settings = function(c) c.sticky = false end,
    height = 0.382 }
local revelation = require("revelation")
revelation.init()

local globalkeys = modules.global_keys {
    gears = gears,
    awful = awful,
    quake = quake,
    revelation = revelation,
    cyclefocus = cyclefocus,
    machi = machi,
    hotkeys_popup = hotkeys_popup,
    modkey = modkey
}

modules.tags(conf, modkey, globalkeys)

local clientkeys = modules.client_keys {
    gears = gears,
    awful = awful,
    modkey = modkey
}



clientbuttons = gears.table.join(
    awful.button({}, 1, function(c)
        c:emit_signal("request::activate", "mouse_click", { raise = true })
    end),
    awful.button({ modkey }, 1, function(c)
        c:emit_signal("request::activate", "mouse_click", { raise = true })
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function(c)
        c:emit_signal("request::activate", "mouse_click", { raise = true })
        awful.mouse.client.resize(c)
    end)
)

-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = modules.rules(conf, clientkeys)

for _, rule in ipairs(conf.rules) do
    table.insert(awful.rules.rules, rule)
end
-- }}}
