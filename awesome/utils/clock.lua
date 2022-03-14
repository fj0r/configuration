-- Create a textclock widget
local wibox = require'wibox'
local awful = require'awful'
local beautiful = require'beautiful'
local gears = require'gears'
local naughty = require'naughty'


local dbg = function(x)
    local aaa = ""
    for k, a in ipairs(x) do
        aaa = aaa .. ', ' .. k .. ' = ' .. tostring(a)
    end
    naughty.notify{text=aaa}
end

local clock = {
    forced_num_cols = 3,
    forced_num_rows = 13,
    homogeneous     = false,
    expand          = true,
    forced_height   = 90,
    spacing         = 1,
    layout = wibox.layout.grid
}

local total = clock.forced_num_rows * clock.forced_num_cols

local group = {2,2,2,1,2,2,2}
for k, v in ipairs(group) do
    if k==1 then
        group[k] = v * clock.forced_num_cols
    else
        group[k] = v * clock.forced_num_cols + group[k - 1]
    end
end

-- dbg(group)

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

local time_names = {'y', 'm', 'd', 'w', 'H', 'M', 'S'}
local agot = {}
local agob = {}
for _, v in ipairs(time_names) do
    local t = tonumber(os.date('%'..v))
    table.insert(agot, t)
    tobitarray(agob, t, v == 'w' and 3 or 6)
end
for i = 1, total do
    clock[i].checked = agob[i]
end

gears.timer {
    timeout   = 1,
    call_now  = true,
    autostart = true,
    callback  = function()
        local begin_bit = 0
        local bits = {}
        for i = #time_names, 1, -1 do
            local t = tonumber(os.date('%'..time_names[i]))
            if t == agot[i] then
                break
            end
            agot[i] = t
            begin_bit = group[i-1]
            for x = 0, time_names[i] == 'w' and 3 or 6, 1 do
                table.insert(bits, 1, (t & (1 << x)) ~= 0)
            end
        end
        for i = begin_bit + 1, total, 1 do
            clock[i].checked = bits[i - begin_bit]
        end
        --dbg{begin_bit, total}
        --dbg(bits)
        --local vls = {}
        --for k, v in ipairs(time_names) do
        --    tobitarray(vls, tonumber(os.date('%'..v)), v == 'w' and 3 or 6)
        --end
        --for i = 1, total do
        --    if clock[i].checked ~= vls[i]
        --    then clock[i].checked = vls[i]
        --    end
        --end
    end
}

return  c

