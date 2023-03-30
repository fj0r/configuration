local beautiful = require("beautiful")
local awful = require("awful")

return function(conf, clientkeys)
    return {
        -- All clients will match this rule.
        { rule = {}
            , properties = { border_width = conf.theme.border.width or beautiful.border_width
                , border_color = beautiful.border_normal
                , focus = awful.client.focus.filter
                , raise = true
                , keys = clientkeys
                , buttons = clientbuttons
                , screen = awful.screen.preferred
                , placement = awful.placement.no_overlap + awful.placement.no_offscreen
            }
        },


        { rule = { class = 'MPlayer' }
            , properties = { floating = true
                , ontop = true
                , placement = awful.placement.centered
            }
        },

        -- Floating clients.
        { rule_any = conf.floating
            , properties = { floating = true
                , ontop = true
                , placement = awful.placement.centered
            }
        },

        -- Add titlebars to normal clients and dialogs
        { rule_any = { type = { "normal", "dialog" } }
            , properties = { titlebars_enabled = false }
        },

        -- Set Firefox to always map on the tag named "2" on screen 1.
        -- { rule = { class = "Firefox" },
        --   properties = { screen = 1, tag = "2" } },
    }
end
