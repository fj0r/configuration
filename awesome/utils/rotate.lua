local wibox = require("wibox")

local rotated_widget = function(w)
    local c = wibox.container.rotate()
    c:set_direction("west")
    c:set_widget(w)
    return c
end

return rotated_widget