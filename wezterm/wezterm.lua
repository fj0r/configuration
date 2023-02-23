local wezterm = require 'wezterm'
local mux = wezterm.mux
wezterm.on("gui-startup", function()
  local tab, pane, window = mux.spawn_window {}
  window:gui_window():maximize()
end)

return {
    default_prog = { "tmux" },
    color_scheme = "Gruvbox Dark",
    enable_tab_bar = false,
    window_decorations = "RESIZE",
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
