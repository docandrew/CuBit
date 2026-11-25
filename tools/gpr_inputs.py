#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Write a make rule naming what a gprbuild step read (BLD-001, interim).

The programs not yet built by ccl-build are linked by gprbuild, which is
its own up-to-date check but costs a second or more even when nothing
changed. After such a step, kernel/Makefile runs this with the step's
stamp and its gprls command line (`gprls -d -s -P PROJECT ...`, the same
project and -X values): DEPFILE then says the stamp depends on

  - every Ada source gprls lists, from the project and its imports;
  - every other file in those sources' directories (the C sources and
    headers beside them), and the directories themselves, so a source
    added or removed is seen;

each named relative to make's directory, with an empty rule for each so a
deleted source makes the step run again rather than fail. What the step
reads besides (the runtime, the C start file, the linker script, the
manifest, the compilers, -X values) the Makefile names itself.
"""
import os
from pathlib import Path
import subprocess
import sys


def main():
    if len(sys.argv) < 5 or sys.argv[3] != "--":
        raise SystemExit("usage: gpr_inputs.py DEPFILE TARGET -- GPRLS-COMMAND...")
    depfile, target, command = Path(sys.argv[1]), sys.argv[2], sys.argv[4:]
    result = subprocess.run(command, capture_output=True, text=True)
    sources = set()
    for line in result.stdout.splitlines():
        for word in line.split():
            if word.startswith("/") and os.path.isfile(word):
                sources.add(os.path.realpath(word))
    if not sources:
        raise SystemExit(f"gpr_inputs: {' '.join(command)} listed no sources:\n{result.stderr.strip()}")
    names = set()
    for source in sources:
        directory = os.path.dirname(source)
        names.add(directory)
        for entry in os.scandir(directory):
            if entry.is_file():
                names.add(entry.path)
    relative = sorted(os.path.relpath(name).replace(" ", "\\ ") for name in names)
    pending = depfile.with_name(depfile.name + f".{os.getpid()}.tmp")
    depfile.parent.mkdir(parents=True, exist_ok=True)
    pending.write_text(f"{target}: \\\n" + "".join(f"  {name} \\\n" for name in relative) + "\n"
                       + "".join(f"{name}:\n" for name in relative))
    os.replace(pending, depfile)


if __name__ == "__main__":
    main()
