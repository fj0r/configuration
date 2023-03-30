return {
    monitor     = require("utils.monitor"),
    clock       = require("utils.clock"),
    rotate      = require('utils.rotate'),
    script_path = require('utils.script_path'),
    mk_taglist  = require('utils.taglist'),
    run_once    = require('utils.run_once'),
    centerwork  = require('utils.centerwork'),
    say         = function(message)
        local naughty = require("naughty")
        local inspect = require('utils.inspect')
        naughty.notify {
            titel = 'inspect',
            timeout = 0,
            opacity = 0.5,
            --position = 'top_middle',
            bg = '#FBFFB9',
            fg = 'black',
            text = inspect(message)
        }
    end
}
