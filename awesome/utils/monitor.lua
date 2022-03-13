local wibox = require("wibox")
local lain = require("lain")
local awful = require("awful")
local beautiful = require("beautiful")

local function hover(config, obj, m)
    obj.tooltip.markup = config.id .. ': ' .. tostring(config.metrics()) .. ' / ' .. m.max_value
    obj.tooltip.visible = true
end

local default_tooltip = {
    mode = 'outside',
    preferred_positions = {'left', 'right'},
    preferred_alignments = 'middle',
    margins = 6,
    border_width = 2,
    bg = beautiful.tooltip_bg
}

local function new_monitor(config)
    local m = wibox.widget {
        max_value = config.max_value,
        scale = config.scale,
        color = config.color,
        widget = wibox.widget.graph,
    }
    local o = {
        tooltip = awful.tooltip(default_tooltip)
    }
    config.src {
        settings = function()
            m:add_value(config.metrics())
        end
    }
    m:connect_signal('mouse::enter', function() hover(config, o, m) end)
    m:connect_signal('mouse::leave', function() o.tooltip.visible = false end)
    return wibox.container.mirror(m, { horizontal = true, vertical = config.reflection or false })
end

local function new_pie(config)
    local m = wibox.widget {
        colors = {config.color},
        min_value = 0,
        max_value = config.max_value or 100,
        rounded_edge = false,
        start_angle = 0,
        paddings = 2,
        thickness = 4,
        bg = '#888',
        border_width = 0,
        border_color = '#fff',
        widget = wibox.container.arcchart,
        align = "center",
        valign = "center",
    }
    local o = {
        tooltip = awful.tooltip(default_tooltip)
    }
    config.src {
        settings = function()
            m.value = config.metrics()
        end
    }
    m:connect_signal('mouse::enter', function() hover(config, o, m) end)
    m:connect_signal('mouse::leave', function() o.tooltip.visible = false end)
    return wibox.container.mirror(m, { horizontal = true })
end

local function new_fschart(config)
    local fs_text = wibox.widget {
        text   = '',
        align  = "center",
        valign = "center",
        widget = wibox.widget.textbox,
    }
    local colors = {}
    for k, v in ipairs(config.colors) do
        table.insert(colors, v)
        table.insert(colors, config.default_color or '#888')
    end
    local fs_chart = wibox.widget {
        fs_text,
        colors = colors,
        values = {},
        max_value    = 100,
        min_value    = 0,
        rounded_edge = false,
        start_angle = 0,
        paddings      = 2,
        thickness    = 4,
        bg           = "#888",
        border_width = 0,
        border_color = "#000000",
        widget       = wibox.container.arcchart,
        align  = "center",
        valign = "center",
    }
    lain.widget.fs({
        settings  = function()
            local total = 0
            local values = {}
            for k, v in ipairs(config.partitions) do
                local p = fs_now[v]
                table.insert(values, (p.size - p.free) * 100)
                table.insert(values, p.free * 100)
                total = total + p.size
            end
            for k, v in ipairs(values) do
                values[k] = v / total
            end
            fs_chart.values = values
        end
    })
    return wibox.container.mirror(fs_chart, { horizontal = true })
end


local cpu = new_monitor {
    id = 'CPU',
    max_value = 100,
    color = '#728639',
    src = lain.widget.cpu,
    metrics = function() return cpu_now.usage end
}

local mem = new_monitor {
    id = 'MEM',
    max_value = 100,
    color = '#1071b0',
    src = lain.widget.mem,
    metrics = function() return mem_now.perc end
}

local net_up = new_monitor {
    id = 'NET UP',
    max_value = 1024,
    color = '#ff964f',
    src = lain.widget.net,
    metrics = function() return net_now.sent + 0 end
}
local net_down = new_monitor {
    id = 'NET DOWN',
    max_value = 10240,
    color = '#08787f',
    reflection = true,
    src = lain.widget.net,
    metrics = function() return net_now.received + 0 end
}

local battery = new_pie {
    id = 'BATTERY',
    max_value = 100,
    color = '#ffd8b1',
    src = lain.widget.bat,
    metrics = function() return bat_now.perc end
}

local fsw = function(config)
    local fs = {}

    local p = io.popen("lsblk -ln | awk '{print $7}' | awk NF")
    for s in p:lines() do
        for k1,v1 in ipairs(config.partitions) do
            if v1 == s then
                table.insert(fs, s)
            end
        end
    end
    return wibox.widget {
        new_fschart {
            partitions = fs,
            colors = config.colors or {'#e17701', '#c65102', '#ffd8b1'},
            default_color = config.default_colors or '#666',
        },
        layout = wibox.layout.fixed.vertical,
        align = 'center',
        valign = 'center',
        paddings = 3,
    }
end

return function(config)
    return wibox.widget {
            layout  = wibox.layout.fixed.vertical,
            battery,
            cpu,
            mem,
            net_up,
            net_down,
            fsw(config),
        }
end
