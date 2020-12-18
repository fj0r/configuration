local M = {}
local awful = require("awful")
local yaml = require('utils.tinyyaml')
local get = require('utils.get')

function M.gen_config(path)
    local file = io.open(path, 'rb')
    local content = file:read("*a")
    file:close()
    local conf = yaml.parse(content)
    -- local conf = require(path)
    local tags = {}
    conf.layouts = {}
    conf.rules = {}
    for k, v in ipairs(conf.tags) do
        -- table.insert(tags, k .. "|" .. v.name)
        table.insert(tags, v.name)
        table.insert(conf.layouts, v.layout and #v.layout ~= 0
                and get(awful.layout.suit, v.layout)
                or awful.layout.suit.tile)
        for _, term in ipairs(v.apps or {}) do
            local rule = {}
            local count = 0
            for p in ipairs({'instance', 'class', 'name', 'role'}) do
                if term[p] then
                    rule[p] = term[p]
                    term[p] = nil
                    count = count + 1
                end
            end
            if count > 0 then
                term.screen = term.screen or 1
                term.tag = k
                table.insert(conf.rules, {
                    rule = rule,
                    properties = term
                })
            end
        end
    end
    conf.tags = tags
    return conf
end

return M