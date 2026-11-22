from pathlib import Path
import json
import re
import subprocess
import sys

out = Path(sys.argv[1]).resolve()
oracle = '/nix/store/72fphm0m9k2f8zd066ayxkqi44ia9ql5-acpica-tools-20260408/bin/acpiexec'
commands = {
    'source': ('evaluate SCP0.I2C0.MARK; evaluate I2C0.MARK; evaluate scp0.als0.mark; evaluate SCP0.DEV0.I2C0', [2,1,2,3]),
    'hid': ('evaluate OREG; evaluate SETM 9; evaluate OREG; evaluate SETM 10; evaluate OREG; evaluate SETM 8; evaluate TPD0._STA; evaluate SETM 11; evaluate TPD0._STA; evaluate SETM 12; evaluate TPD0._STA', [32,0,65535,13,0,3]),
    'cache': ('evaluate ORCL; evaluate SELM 16; evaluate ORCL', [1,42000,1,100,20000,0,42000,1,100,20000]),
    'observer': ('evaluate ORCL; evaluate SELM 16; evaluate ORCL', [1,42000,1,100,20000,0,42000,1,100,20000]),
    'groups': ('evaluate ORCL; evaluate SELM 16; evaluate ORCL', [1,42000,1,100,20000,0,42000,1,100,20000]),
    'lid': ('evaluate GETV; evaluate LID0._LID; evaluate SETV 0x80; evaluate LID0._LID', [90,90,128])}
selected = []; notifications = []
for width in (1, 2):
    for fixture, (command, expected) in commands.items():
        log = out / f'oracle-{fixture}-{width}.log'
        with log.open('wb') as stream:
            subprocess.run(['timeout', '--kill-after=2s', '5', oracle, '-dr', '-b', command,
                            str(out / f'{fixture}-{width}.aml')], stdout=stream, stderr=subprocess.STDOUT, check=True)
        assert log.stat().st_size <= 1024 * 1024
        text = log.read_text()
        assert not re.search(r'failed with status|Aborting method', text)
        actual = [int(value, 16) for value in re.findall(r'\[Integer\]\s*=\s*([0-9A-Fa-f]+)', text)]
        assert actual == expected, (fixture, actual)
        selected.extend(actual)
        if fixture == 'lid':
            events = [[name, int(value,16)] for name,value in re.findall(r'Received a Device Notify on \[([A-Z0-9_]{4})\].*?Value 0x([0-9A-Fa-f]+)', text)]
            assert events == [['LID0',128]], events
            notifications.extend(events)
(out / 'oracle-observations.json').write_text(json.dumps({'selected_integers': selected, 'lid_notifications': notifications}, indent=2) + '\n')
assert len(selected) == 86 and len(notifications) == 2
print('Unified ACPICA observations matched:86selected integers+2lid notifications.', flush=True)
