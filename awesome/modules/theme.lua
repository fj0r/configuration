local beautiful = require("beautiful")
local gears = require("gears")

return function(conf)
    -- {{{ Variable definitions
    -- Themes define colours, icons, font and wallpapers.
    beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
    beautiful.useless_gap = conf.theme.gap

    local color_normal = conf.theme.color.normal.bg or beautiful.border_normal
    local color_normal_fg = conf.theme.color.normal.fg or beautiful.border_normal
    local color_focus = conf.theme.color.focus.bg or beautiful.border_focus
    local color_focus_fg = conf.theme.color.focus.fg or beautiful.border_focus
    local color_urgent = conf.theme.color.urgent_color
    beautiful.bg_normal = color_normal
    beautiful.bg_focus = color_focus
    beautiful.taglist_bg_normal = color_normal
    beautiful.taglist_bg_focus = color_focus
    beautiful.tasklist_bg_normal = color_normal
    beautiful.tasklist_bg_focus = color_focus

    -- Enable sloppy focus, so that focus follows mouse.
    client.connect_signal("mouse::enter", function(c)
        c:emit_signal("request::activate", "mouse_enter", { raise = false })
    end)

    client.connect_signal("focus", function(c)
        c.border_color = color_focus
    end)
    client.connect_signal("unfocus", function(c)
        if c.urgent then return end
        c.border_color = color_normal
    end)
    client.connect_signal("property::urgent", function(c)
        c.border_color = color_urgent
    end)
end
