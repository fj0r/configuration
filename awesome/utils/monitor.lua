local wibox = require("wibox")
local lain = require("lain")
local awful = require("awful")
local beautiful = require("beautiful")
local say = require('say')

local attach_tooltip = function(obj, fun)
    return awful.tooltip {
            objects = { obj },
            mode = 'outside',
            preferred_positions = { 'left', 'right' },
            preferred_alignments = 'middle',
            margins = 6,
            border_width = 2,
            bg = beautiful.tooltip_bg,
            timer_function = fun
        }
end

local function new_dual(config)
    local widgets = {}
    for k = #config.metrics, 1, -1 do
        local v = config.metrics[k]
        local g = wibox.widget {
            max_value = config.max_value or 100,
            scale = config.scale,
            color = v.color,
            widget = wibox.widget.graph,
            width = 60,
        }
        table.insert(widgets, k % 2 == 1 and g or wibox.container.mirror(g, { vertical = true }))
        v.src {
            settings = function()
                g:add_value(v.value())
            end
        }
    end
    widgets.layout = wibox.layout.stack
    local m = wibox.widget(widgets)

    attach_tooltip(m, function ()
        local text = ''
        local len = #config.metrics
        for k, v in ipairs(config.metrics) do
            local sep = k == len and '' or '\n'
            text = text .. v.format(v.value()) .. sep
        end
        return text
    end)
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
    x = wibox.container.mirror(m, { horizontal = true })
    attach_tooltip(x, function ()
        return config.id .. ': <b>' .. tostring(config.value()) .. '</b> / ' .. '%'
    end)
    return x
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
    metrics = {
        {
            color = '#728639',
            src = lain.widget.cpu,
            value = function() return cpu_now.usage end,
            format = function(v) return 'cpu: <b>' .. v .. '</b>%' end,
        },
        {
            color = '#1071b0',
            src = lain.widget.mem,
            value = function() return mem_now.perc end,
            format = function(v) return 'mem: <b>' .. v .. '</b>%' end,
        }
    }
}

local two_digit = function(f)
    return math.ceil(f * 100) / 100
end
local gb = 1024 * 1024
local mb = 1024
local format_net = function(txt, total)
    return function(v)
        local value = v
        local unit = 'K'
        if v > gb then
            value = v / gb
            unit = 'G'
        elseif v > mb then
            value = v / mb
            unit = 'M'
        end
    return txt .. ': <b>' .. two_digit(value) .. unit .. '</b>/' .. total
    end
end

local net = function(config)
    local bandwidth = config.bandwidth or 10
    return new_dual {
        max_value = bandwidth * mb,
        metrics = {
            {
                color = '#08787f',
                reflection = true,
                src = lain.widget.net,
                value = function() return net_now.received + 0 end,
                format = format_net('down', bandwidth .. 'M')
            },
            {
                color = '#ff964f',
                src = lain.widget.net,
                value = function() return net_now.sent + 0 end,
                format = format_net('up', bandwidth .. 'M')
            }
        }
    }
end

local battery = new_pie {
    id = 'BATTERY',
    max_value = 100,
    color = '#ffd8b1',
    src = lain.widget.bat,
    value = function() return bat_now.perc end
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
        rotate(cpu_mem),
        fsw(config),
        rotate(net(config)),
        battery,
    }
end
