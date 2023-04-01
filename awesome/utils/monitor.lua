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

    attach_tooltip(m, function()
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

local cpu_mem = function()
    return new_dual {
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
                format = function(v) return 'mem: <b>' .. v .. '</b>%(<b>' .. mem_now.used .. '</b>M)' end,
            }
        }
    }
end


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
        values = {}
    }
    local x = wibox.container.mirror(m, { horizontal = true })
    config.src {
        settings = function()
            local v = config.value()
            if v == 0 then
                m.colors[1] = config.invalid_color or 'black'
            end
            m.values[1] = v
        end
    }
    attach_tooltip(x, function()
        return config.format(config.value())
    end)
    return x
end

local battery = function()
    return new_pie {
        color = '#ffd8b1',
        invalid_color = 'black',
        src = lain.widget.bat,
        value = function() return bat_now.perc == 'N/A' and 0 or bat_now.perc end,
        format = function(v) return v > 0 and 'BATTERY: <b>' .. v .. '</b>%' or 'NO BATTERY' end
    }
end

local function new_fschart(config)
    local fs_text = wibox.widget {
        text   = config.text,
        align  = "center",
        valign = "center",
        widget = wibox.widget.textbox,
    }
    local fs_chart = wibox.widget {
        fs_text,
        colors       = config.colors,
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
    return fs_chart
end

local fstab = function(exclude_prefix)
    local fs = {}
    local p = io.popen("cat /etc/fstab | grep -v '^#' | awk '{print $2}'")
    for f in p:lines() do
        for _, e in ipairs(exclude_prefix or {}) do
            if string.sub(f, 1, string.len(e)) == e then
                goto continue
            end
        end
        table.insert(fs, f)
        ::continue::
    end
    return fs
end

local fsw = function(config)
    local refs = {}
    local x = {
        layout = wibox.layout.fixed.vertical,
        align = 'center',
        valign = 'center',
    }
    local partitions = type(config.partitions) == "table"
        and config.partitions
        or fstab { '/boot', '/dev', '/proc', 'swap', 'none' }

    for _, v in ipairs(partitions) do
        local y = new_fschart {
            values = {},
            colors = {},
            default_color = config.default_colors or '#666',
        }
        table.insert(x, wibox.container.margin(y, 2, 2, 1, 1))
        refs[v] = y
        attach_tooltip(y, function() return refs[v].tooltip end)
    end
    local color_list = config.colors or { '#f7aa97', '#ed9282', '#de7e73', '#af4034', '#d81159' }
    lain.widget.fs {
        settings = function()
            for _, v in ipairs(partitions) do
                local p = fs_now[v]
                refs[v].values = { p.percentage }
                refs[v].tooltip = '<b>' ..
                    v .. '</b>: ' .. two_digit(p.free) .. p.units .. ' free, ' .. p.percentage .. '%'
                local ic = math.ceil(p.percentage / (100 / #color_list))
                local c = color_list[ic]
                refs[v].colors = { c }
            end
        end
    }
    return wibox.widget(x)
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
        wibox.container.margin(rotate(cpu_mem()), 0, 0, 2, 1),
        wibox.container.margin(rotate(net(config)), 0, 0, 0, 2),
        wibox.container.margin(battery(), 2, 2, 1, 1),
        fsw(config),
    }
end
