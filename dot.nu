$env.NVIM_LEVEL = if 'NVIM_LEVEL' in $env { $env.NVIM_LEVEL } else { 'x' }
$env.NVIM_REMOTE_HISTORY = $"($env.HOME)/.cache/nvim_history.sqlite"
$env.PREFER_ALT = '1'
$env.LIBRETRANSLATE_HOST = 'http://localhost:5000'
$env.NVIM_FONT = "nar11"
$env.NVIM_LIGHT = $env.NVIM_LIGHT? | default '1'
# $env.NVIM_ARROW = 1
$env.EDITOR = 'nvim'
$env.OLLAMA_BASEURL = 'http://172.178.5.123:11434'
$env.MARKDOWN_RENDER = 'glow'
$env.MARKDOWN_TRANSFORM.summary-zh = { $in | ad text-summary zh -o }

#$env.NEOVIM_LINE_SPACE = '0'
$env.NEOVIDE_SCALE_FACTOR = '1.0'

$env.RUSTC_WRAPPER = '/usr/local/bin/sccache'

# use resolvenv
# resolvenv select wlan0 [
#     [{screen: {port: 'hdmi'}, wifi: 'pandorabox'} {
#         NEOVIM_LINE_SPACE: '0'
#         NEOVIDE_SCALE_FACTOR: '1'
#     }]
#     [{screen: {port: 'eDP'}} {
#         NEOVIM_LINE_SPACE: '0'
#         NEOVIDE_SCALE_FACTOR: '1'
#     }]
#     [_ { print ($in | table -e) }]
# ]

#$env.lg.file = ~/.cache/nonstdout

#source ~/.config/ai.config.nu
$env.AI_TOOLS.web_search.context.proxy = 'http://localhost:7890'
$env.AI_TOOLS.web_fetch.context.proxy = 'http://localhost:7890'
