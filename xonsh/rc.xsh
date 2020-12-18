import os, sys
import re as regexp
import json
import random
from functools import reduce
from datetime import datetime

sys.path.insert(0, f'{os.path.dirname(__file__)}')

from lib import \
( common       as ____common
, alias        as ____alias
, keybindings  as ____keybindings
, prompt       as ____prompt
, styles       as ____styles
, git          as ____git
, now          as ____now
, just         as ____just
, docker_ext   as ____docker_ext
, awesome      as ____awesome
)

xontrib load autoxsh

# TODO:
# [+] prompt yellow
# [+] import utils
# [ ] alias dr
# [ ] named dir
#     [ ] custom space key
# [ ] c-q
# [ ] completion _describe
# [ ] default shell
# [ ] vim



