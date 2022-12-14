local wezterm = require 'wezterm'

return {
    color_scheme = "Gruvbox Dark",
    enable_tab_bar = false,
    font = wezterm.font {
        family = 'JetBrainsMono',
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
