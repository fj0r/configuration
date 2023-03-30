local beautiful = require("beautiful")
local gears = require("gears")

return function(conf)
    -- {{{ Variable definitions
    -- Themes define colours, icons, font and wallpapers.
    beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
    beautiful.useless_gap = conf.theme.gap
    local color_normal = conf.theme.border.normal and conf.theme.border.normal or beautiful.border_normal
    local color_focus = conf.theme.border.focus and conf.theme.border.focus or beautiful.border_focus
    local color_urgent = conf.theme.urgent_color
    beautiful.taglist_bg_urgent = color_urgent

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
