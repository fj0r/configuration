local wibox = require("wibox")
local lain = require("lain")
local awful = require("awful")
local beautiful = require("beautiful")

local default_tooltip = {
    mode = 'outside',
    preferred_positions = { 'left', 'right' },
    preferred_alignments = 'middle',
    margins = 6,
    border_width = 2,
    bg = beautiful.tooltip_bg
}

local function hover(config, obj)
    obj.tooltip.markup = config.id .. ': ' .. tostring(config.metrics()) .. ' / ' .. config.max_value
    obj.tooltip.visible = true
end

local function hover_dual(config, obj)
    local text = ''
    for k, v in ipairs(config.metrics) do
        local sep = k == #config.metrics and '' or '\n'
        text = text .. v.id .. ': ' .. tostring(v.metrics()) .. ' / ' .. config.max_value .. sep
    end
    obj.tooltip.markup = text
    obj.tooltip.visible = true
end

local function new_dual(config)
    local widgets = {}
    for k = #config.metrics, 1, -1 do
        local v = config.metrics[k]
        local g = wibox.widget {
            max_value = config.max_value,
            scale = config.scale,
            color = v.color,
            widget = wibox.widget.graph,
            width = 60,
        }
        table.insert(widgets, k % 2 == 1 and g or wibox.container.mirror(g, { vertical = true }))
        v.src {
            settings = function()
                g:add_value(v.metrics())
            end
        }
    end
    widgets.layout = wibox.layout.stack
    local m = wibox.widget(widgets)
    local o = {
        tooltip = awful.tooltip(default_tooltip)
    }
    m:connect_signal('mouse::enter', function() hover_dual(config, o) end)
    m:connect_signal('mouse::leave', function() o.tooltip.visible = false end)
    return m
end

local function new_pie(config)
    local m = wibox.widget {
        colors = { config.color },
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
    m:connect_signal('mouse::enter', function() hover(config, o) end)
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
        colors       = colors,
        values       = {},
        max_value    = 100,
        min_value    = 0,
        rounded_edge = false,
        start_angle  = 0,
        paddings     = 2,
        thickness    = 4,
        bg           = "#888",
        border_width = 0,
        border_color = "#000000",
        widget       = wibox.container.arcchart,
        align        = "center",
        valign       = "center",
    }
    lain.widget.fs({
        settings = function()
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

local cpu_mem = new_dual {
    max_value = 100,
    metrics = {
        {
            id = 'cpu',
            color = '#728639',
            src = lain.widget.cpu,
            metrics = function() return cpu_now.usage end
        },
        {
            id = 'mem',
            color = '#1071b0',
            src = lain.widget.mem,
            metrics = function() return mem_now.perc end
        }
    }
}

local net = new_dual {
    max_value = 10240,
    metrics = {
        {
            id = 'down',
            color = '#08787f',
            reflection = true,
            src = lain.widget.net,
            metrics = function() return net_now.received + 0 end
        },
        {
            id = 'up',
            color = '#ff964f',
            src = lain.widget.net,
            metrics = function() return net_now.sent + 0 end
        }
    }
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
        for k1, v1 in ipairs(config.partitions) do
            if v1 == s then
                table.insert(fs, s)
            end
        end
    end
    return wibox.widget {
        new_fschart {
            partitions = fs,
            colors = config.colors or { '#e17701', '#c65102', '#ffd8b1' },
            default_color = config.default_colors or '#666',
        },
        layout = wibox.layout.fixed.vertical,
        align = 'center',
        valign = 'center',
        paddings = 3,
    }
end

local rotate = function(w)
    local c = wibox.container.rotate()
    c:set_direction('west')
    c:set_widget(w)
    return c
end

return function(config)
    return wibox.widget {
        layout = wibox.layout.fixed.vertical,
        fsw(config),
        rotate(cpu_mem),
        rotate(net),
        battery,
    }
end
