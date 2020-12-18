def mk_alias(d):
    for (a, cmd) in d.items():
        aliases[a] = cmd #type(cmd)==str and cmd.split(' ') or cmd

import re
space_split_regext = re.compile(' +')

from functools import wraps
from xonsh.ptk2 import key_bindings as kbs
def intercept_return(fn):
    @wraps(fn)
    def inner(event):
        b = event.cli.current_buffer
        fn(b)
        kbs.carriage_return(b, event.cli)
    return inner

def intercept_tab(fn):
    @wraps(fn)
    def inner(event):
        b = event.cli.current_buffer
        fn(b)
        if not b.complete_state:
            b.start_completion()
    return inner