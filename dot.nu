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

let expert = {
    rust: '你是Rust专家，'
    js: '你是javascript专家，'
    python: '你是Python专家，'
    nushell: '你是Nushell专家，'
}
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
| insert 'git-summary-zh' {
    prompt: [
        "从git的差异中总结提取出提交日志，只总结文件内容变化，忽略hash变化"
        "```"
        "{}"
        "```"
    ]
    model: 'qwen-max'
}
| insert 'synonyms' {
    prompt: [
        "解释以下词语的区别，并介绍相关的近义词和反义词"
        "```{}```"
    ]
    model: 'qwen-max'
    description: '近义词解析'
}
| insert 'api-doc-zh' {
    prompt: [
        "{} 查询API的用法并给出示例"
        "```"
        "{}"
        "```"
    ]
    placeholder: [ $expert ]
    description: 'api 文档查询'
    model: 'qwen-max'
}

source ~/.config/qwen-key.nu