local gears = require("gears")
local awful = require("awful")

return function(conf, modkey, globalkeys)
    -- Bind all key numbers to tags.
    -- Be careful: we use keycodes to make it work on any keyboard layout.
    -- This should map on the top row of your keyboard, usually 1 to 9.
    for i = 1, 10 do
        globalkeys = gears.table.join(globalkeys,
            -- View tag only.
            -- default: { modkey }, "#" .. i + 9
            awful.key({ modkey }, "#" .. i + 9,
                function()
                    local screen = awful.screen.focused()
                    local tag = screen.tags[i]
                    if tag then
                        tag:view_only()
                    end
                end,
                { description = "view tag #" .. i, group = "tag" }),
            -- Move client to tag.
            -- default: { modkey, "Shift" }, "#" .. i + 9
            awful.key({ modkey }, "F" .. i,
                function()
                    if client.focus then
                        local tag = client.focus.screen.tags[i]
                        if tag then
                            client.focus:move_to_tag(tag)
                        end
                    end
                end,
                { description = "move focused client to tag #" .. i, group = "tag" }),
            -- Toggle tag display.
            -- default: { modkey, "Control", "Shift" }, "#" .. i + 9
            awful.key({ "Mod1" }, "F" .. i,
                function()
                    local screen = awful.screen.focused()
                    local tag = screen.tags[i]
                    if tag then
                        awful.tag.viewtoggle(tag)
                    end
                end,
                { description = "toggle tag #" .. i, group = "tag" }),
            -- Toggle tag on focused client.
            awful.key({ "Shift" }, "F" .. i,
                function()
                    if client.focus then
                        local tag = client.focus.screen.tags[i]
                        if tag then
                            client.focus:toggle_tag(tag)
                            tag:view_only()
                        end
                    end
                end,
                { description = "toggle focused client on tag #" .. i, group = "tag" })
        )
    end
end
