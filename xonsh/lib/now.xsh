from datetime import datetime

@events.on_precommand
def add_to_file(cmd: str):
    $NOW = datetime.now().strftime('%y%m%d%H%M%S')