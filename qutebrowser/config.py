c.content.proxy = 'socks://localhost:7891'
c.window.hide_decoration = True
c.editor.command = ['neovide', '--nofork', '{file}', '--', '-c', 'normal {line}G{column0}l']
c.url.searchengines = { 'DEFAULT': 'https://www.google.com/search?q={}'}
c.url.start_pages = 'https://google.com'
c.auto_save.session = True
c.session.lazy_restore = True
c.aliases['r'] = 'session-load'
c.hints.chars = 'fjdksla;rueiwoqpty'
c.hints.scatter = False
c.fonts.hints = 'bold default_size "JetBrains Mono"'
c.fonts.default_size = '8pt'

c.fonts.web.family.standard= 'Noto Sans CJK SC'
c.fonts.web.family.fixed= 'JetBrains Mono'
c.fonts.web.family.sans_serif= 'Jetbrains Mono'
c.fonts.web.family.serif= 'Noto Serif CJK SC'

config.bind('<Alt-o>', 'edit-text')
config.bind('<Ctrl-o>', 'edit-url')
config.bind('zl', 'spawn --userscript qute-bitwarden')

config.load_autoconfig()

