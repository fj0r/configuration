$PROMPT = (
    '{env_name}' +
    '{BOLD_GREEN}{user}@{hostname}{BOLD_BLUE} {cwd}' +
    '{gitstatus: [{}]}{NO_COLOR}' +
    '\n{BOLD_BLUE}{prompt_end}{NO_COLOR}' +
    '{BOLD_YELLOW} >>> {NO_COLOR}'
)

from datetime import datetime

$RIGHT_PROMPT = lambda: datetime.now().strftime('%y-%m-%d %H:%M:%S')

#$UPDATE_COMPLETIONS_ON_KEYPRESS=True
#$PROMPT_REFRESH_INTERVAL=1