from prompt_toolkit.keys import Keys
from prompt_toolkit.filters import Condition, EmacsInsertMode, ViInsertMode
from prompt_toolkit.application.current import get_app
import functools, os, re
from lib.utils import intercept_return, intercept_tab

@Condition
def blank_line():
    return get_app().current_buffer.text == ''

@Condition
def one_space_line():
    return get_app().current_buffer.text == ' '

@Condition
def two_space_line():
    return get_app().current_buffer.text == '  '

@Condition
def many_dots():
    return re.match(r".*\.{3,}.*", get_app().current_buffer.text)

@events.on_ptk_create
def custom_keybindings(bindings, **kw):
    # prompt_toolkit 1.x
    # handler = bindings.registry.add_binding
    # prompt_toolkit 2.x
    handler = bindings.add

    @handler(Keys.Enter, filter=blank_line)
    @intercept_return
    def enter(b):
        b.text = 'ls'

    @handler(Keys.Enter, filter=one_space_line)
    @intercept_return
    def enter(b):
        b.text = 'ls -lh'

    @handler(Keys.Enter, filter=two_space_line)
    @intercept_return
    def enter(b):
        b.text = 'ls -lah'

    @handler(Keys.Enter, filter=many_dots)
    @intercept_return
    def enter(b):
        b.text = re.sub(r'\.{3,}', lambda x: '/'.join(['..']*(len(x.group())-1)) , b.text)

    @handler(Keys.Tab, filter=blank_line)
    @intercept_tab
    def tab(b):
        b.insert_text('cd ')


    @handler(Keys.Tab, filter=one_space_line)
    @intercept_tab
    def tab(b):
        b.text = ''
        b.insert_text('just ')


