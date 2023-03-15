local wezterm = require 'wezterm'
local mux = wezterm.mux
wezterm.on("gui-startup", function()
  local tab, pane, window = mux.spawn_window {}
  window:gui_window():maximize()
end)

return {
    color_scheme = "Gruvbox Dark",
    enable_tab_bar = false,
    default_prog = { "nvim" },
    set_environment_variables = {
        SHELL = "/usr/local/bin/nu",
        NVIM_PRESET = "x",
        PATH= "/opt/node/bin:" .. os.getenv("PATH"),
    },
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
    warn_about_missing_glyphs = false,
}
