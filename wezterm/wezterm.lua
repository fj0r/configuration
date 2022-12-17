local wezterm = require 'wezterm'

return {
    default_prog = { "/usr/local/bin/nu" },
    color_scheme = "Gruvbox Dark",
    enable_tab_bar = false,
    window_decorations = "NONE",
    font = wezterm.font {
        family = 'JetBrains Mono',
        weight = 'Light',
        stretch = 'Condensed',
    },
    font_size = 11,
    window_padding = {
        left = 2,
        right = 2,
        top = 0,
        bottom = 0,
    },
    enable_csi_u_key_encoding = true,
    --enable_kitty_keyboard = true,
}
