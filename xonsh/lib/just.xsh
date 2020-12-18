from lib.utils import space_split_regext as reg_space

def _comp_just(prefix, line, begidx, endidx, ctx):
    if line.startswith('just'):
        x = $(just --list)
        r = set()
        for i in x.split('\n'):
            if len(i) > 0 :
                v = reg_space.split(i.strip())
                r.add(v[0])
        return r

completer add comp_just _comp_just