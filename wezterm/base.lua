local wezterm = require 'wezterm'

wezterm.on("gui-startup", function(cmd)
    local tab, pane, window = wezterm.mux.spawn_window(cmd or {})
    window:gui_window():maximize()
end)

return {
    color_scheme = "zenbones_dark",
    enable_tab_bar = false,
    --default_prog = { "nvim" },
    launch_menu = {
        { label = 'neovim', args = { 'nvim' } },
        { label = 'zellij', args = { 'zellij' } },
    },
    set_environment_variables = {
        SHELL = "/usr/local/bin/nu",
        NVIM_PRESET = "x",
        PATH = "/opt/node/bin:" .. os.getenv("PATH"),
    },
    window_decorations = "RESIZE",
    font = wezterm.font {
        family = 'JetBrains Mono',
        weight = 'Light',
        stretch = 'Condensed',
    },
    font_size = 12,
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
