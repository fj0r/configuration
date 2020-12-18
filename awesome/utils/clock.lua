-- Create a textclock widget
local wibox = require'wibox'
local awful = require'awful'
local beautiful = require'beautiful'
local gears = require'gears'

local clock = {
    forced_num_cols = 3,
    forced_num_rows = 13,
    homogeneous     = false,
    expand          = true,
    forced_height   = 90,
    spacing         = 1,
    layout = wibox.layout.grid
}

local col = 3
local row = 13
local total = row * col

local group = {2,2,2,1,2,2,2}
for k, v in ipairs(group) do
    if k==1 then
        group[k] = v * col
    else
        group[k] = v * col + group[k - 1]
    end
end

local color = {}
local color_set = {'#dde175', '#88b555'}
for k, v in ipairs(group) do
    table.insert(color, color_set[k % #color_set + 1])
end

function clock:init()
    for i = 1, total do
        local c
        for k, v in ipairs(group) do
            if i <= v then
                c = color[k]
                break
            end
        end
        table.insert(self, wibox.widget {
            checked       = false,
            paddings = 0,
            color         = c,
            border_color = self.checked and c or '#555',
            shape         = gears.shape.rectangle, -- rectangle octogon
            widget        = wibox.widget.checkbox
        })
    end
    return wibox.widget(self)
end

local c = clock:init()

function tobitarray(r, t, len)
    for i = len - 1, 0, -1 do
        table.insert(r, (t & (1 << i)) ~= 0 )
    end
end

gears.timer {
    timeout   = 8,
    call_now  = true,
    autostart = true,
    callback  = function()
        local vls = {}
        for k, v in ipairs{'y', 'm', 'd', 'w', 'H', 'M', 'S'} do
            tobitarray(vls, tonumber(os.date('%'..v)), v == 'w' and 3 or 6)
        end
        for i = 1, total do
            if clock[i].checked ~= vls[i]
            then clock[i].checked = vls[i]
            end
        end
    end
}

return  c