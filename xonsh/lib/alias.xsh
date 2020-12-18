from lib.utils import mk_alias

mk_alias({
    'rm': 'rm -i',
    'du': 'du -h',
    'df': 'df -h',
    'mkdir': 'mkdir -p',
    'diff': 'diff -u',
    'll': 'ls -alh',
    'r': 'grep --color=auto',
    'v': 'vim',
    'e': 'code',
    'g': 'git',
    's': 'ssh',
    'sa': 'ssh-agent $SHELL',
    'sad': 'ssh-add',
    't': 'tmux',
    'rs': "rsync -avP",
    'px': 'ps aux',
})

