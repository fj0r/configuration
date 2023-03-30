-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")
local utils = require 'utils'
local say = utils.say
local conf = require('conf')
local modules = require 'modules'

-- Standard awesome library
local awful = require("awful")
local beautiful = require("beautiful")
require("awful.autofocus")

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

local machi = require("layout-machi")
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

modules.screen(conf, menu, set_wallpaper)
-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)

modules.mouse()

local globalkeys = modules.global_keys(conf, modkey)
root.keys(globalkeys)

local clientkeys = modules.client_keys(modkey)
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = modules.rules(conf, modkey, clientkeys)

for _, rule in ipairs(conf.rules) do
    table.insert(awful.rules.rules, rule)
end
