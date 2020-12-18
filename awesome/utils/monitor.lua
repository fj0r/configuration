local wibox = require("wibox")
local lain = require("lain")

local function new_monitor(config)
    local m = wibox.widget {
        max_value = config.max_value or 100,
        scale = config.scale,
        color = config.color,
        widget = wibox.widget.graph,
    }
    config.src {
        settings = function()
            m:add_value(config.metrics())
        end
    }
    -- cpu:connect_signal('mouse::enter', function() end)
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
    color = '#63ee00',
    src = lain.widget.cpu,
    metrics = function() return cpu_now.usage end
}

local mem = new_monitor {
    color = '#0366d6',
    src = lain.widget.mem,
    metrics = function() return mem_now.perc end
}

local net = new_monitor {
    color = '#e2692b',
    scale = false,
    max_value = 1024,
    src = lain.widget.net,
    metrics = function() return net_now.sent + net_now.received end
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
            colors = config.colors or {'#e2692b', '#63ee00', '#0366d6'},
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
            cpu,
            mem,
            net,
            fsw(config),
        }
end