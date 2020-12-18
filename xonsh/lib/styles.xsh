from random import choice
styles = [
    'default',
    'lovelace',
    'monokai',
    'murphy',
    'native',
    'paraiso-dark',
    'paraiso-light',
    'sas',
    'solarized-dark',
    'solarized-light',
    'trac',
    'xcode'
]

xi = len($(ps u | grep xonsh).split('\n'))-3
$XONSH_COLOR_STYLE = 'default' #styles[xi % len(styles)]
