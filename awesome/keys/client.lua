return function(t)
    local gears = t.gears
    local awful = t.awful
    local modkey = t.modkey
    local naughty = require("naughty")
    return gears.table.join(
        awful.key({ modkey, "Control" }, "Return",
            function(c)
                c.fullscreen = not c.fullscreen
                c:raise()
            end,
            { description = "toggle fullscreen", group = "client" }),
        awful.key({ modkey, }, "c", function(c) c:kill() end,
            { description = "close", group = "client" }),
        awful.key({ modkey, }, "f", awful.client.floating.toggle,
            { description = "toggle floating", group = "client" }),
        awful.key({ modkey, }, "y", function(c) c:swap(awful.client.getmaster()) end,
            { description = "move to master", group = "client" }),
        awful.key({ modkey, }, ";", function(c) c:move_to_screen() end,
            { description = "move to screen", group = "client" }),
        awful.key({ modkey, }, "t", function(c) c.ontop = not c.ontop end,
            { description = "toggle keep on top", group = "client" }),
        awful.key({ modkey, }, "m",
            function(c)
                c.maximized = not c.maximized
                c:raise()
            end,
            { description = "(un)maximize", group = "client" }),
        awful.key({ modkey, "Control" }, "m",
            function(c)
                c.maximized_vertical = not c.maximized_vertical
                c:raise()
            end,
            { description = "(un)maximize vertically", group = "client" }),
        awful.key({ modkey, "Shift" }, "u", function(c) c.urgent = true end,
            { description = "mark client urgent", group = "client" }),
        awful.key({ modkey, "Shift" }, "m",
            function(c)
                c.maximized_horizontal = not c.maximized_horizontal
                c:raise()
            end,
            { description = "(un)maximize horizontally", group = "client" })
    -- awful.key({ modkey,           }, ",",
    --     function (c)
    --         -- The client currently has the input focus, so it cannot be
    --         -- minimized, since minimized clients can't have the focus.
    --         c.minimized = true
    --     end ,
    --     {description = "minimize", group = "client"}),
    -- awful.key({ modkey, "Control" }, ",",
    --     function ()
    --         local c = awful.client.restore()
    --         -- Focus restored client
    --         if c then
    --           c:emit_signal(
    --               "request::activate", "key.unminimize", {raise = true}
    --           )
    --         end
    --     end,
    --     {description = "restore minimized", group = "client"})
    )
end
