"""Compile a frontend probe with Mesa's exact internal ABI feature defines."""
import json
from pathlib import Path
import shlex
import subprocess
import sys

build, source, output = map(lambda value: Path(value).resolve(), sys.argv[1:4])
commands = json.loads((build / "compile_commands.json").read_text())
entry, = [item for item in commands if item["file"].endswith("/st_manager.c")]
args = shlex.split(entry["command"])
filtered = []
index = 0
while index < len(args):
    if args[index] in ("-o", "-c", "-MF", "-MQ", "-MT"):
        index += 2
    elif args[index] in ("-MD", "-MMD", "-MP"):
        index += 1
    else:
        filtered.append(args[index])
        index += 1
subprocess.run(filtered + sys.argv[4:] + ["-c", str(source), "-o", str(output)],
               cwd=entry["directory"], check=True)
