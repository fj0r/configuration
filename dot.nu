$env.NVIM_LEVEL = if 'NVIM_LEVEL' in $env { $env.NVIM_LEVEL } else { 'x' }
$env.NVIM_REMOTE_HISTORY = $"($env.HOME)/.cache/nvim_history.sqlite"
$env.PREFER_ALT = '1'
$env.LIBRETRANSLATE_HOST = 'http://localhost:5000'
$env.NVIM_FONT = "nar12"
# $env.NVIM_ARROW = 1
$env.EDITOR = 'helix'
$env.OLLAMA_HOST = 'http://gitea.s:11434'
$env.OPENAI_HOST = 'http://gitea.s:11434'

use resolvenv
resolvenv select wlan0 [
    [{screen: {port: 'hdmi'}, wifi: 'pandorabox'} {
        NEOVIM_LINE_SPACE: '0'
        NEOVIDE_SCALE_FACTOR: '1'
    }]
    [{screen: {port: 'eDP'}} {
        NEOVIM_LINE_SPACE: '0'
        NEOVIDE_SCALE_FACTOR: '1'
    }]
    [_ { print ($in | table -e) }]
]

#$env.lg.file = ~/.cache/nonstdout

$env.OPENAI_PROMPT = $env.OPENAI_PROMPT
| insert 'trans-to-zh' {
    prompt: [
        "Translate the following text into Chinese:"
        "```{}```"
    ]
    model: 'qwen2:1.5b',
    description: 'Translation to Chinese'
}
| insert 'json-to-rust' {
    prompt: [
        "Analyze the following JSON data to convert it into a Rust struct:"
        "```{}```"
    ]
    model: '',
    description: 'Analyze JSON content, converting it into a Rust struct'
}

source ~/.config/qwen-key.nu