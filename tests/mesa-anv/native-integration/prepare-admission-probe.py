#!/usr/bin/env python3
"""Install negative admission IPC hooks only in a complete private snapshot.

Run inside `tools/build-workspace.py run`, then build ipctest-server/client and
run the normal async-ipc headless test with a fresh scratch ext2 disk. No
procmgr policy or capability grants are added. The positive delegation probe
requires a separate authorized fixture and is deliberately not claimed here.
"""
import argparse
import hashlib
import json
from pathlib import Path


def prepare(workspace):
    workspace = workspace.resolve(strict=True)
    metadata = json.loads((workspace / '.cubit-build-workspace.json').read_text())
    source = Path(metadata['source']).resolve(strict=True)
    if not metadata.get('complete') or workspace == source:
        raise ValueError('require complete, independent build snapshot')
    if workspace.parent != source / '.build-workspaces':
        raise ValueError('require managed private build snapshot directory')
    receipt = workspace / 'admission-probe-hooks.json'
    if receipt.exists():
        raise ValueError('hooks already prepared; use a fresh snapshot')
    inputs = {item['path']: item for item in metadata['inputs']}
    edits = {}

    def replace(name, old, new):
        path = workspace / name
        if path.resolve() != path or not path.is_file():
            raise ValueError(f'not an ordinary snapshot file: {name}')
        if name not in edits:
            data = path.read_bytes()
            if hashlib.sha256(data).hexdigest() != inputs[name]['sha256']:
                raise ValueError(f'snapshot source already changed: {name}')
            edits[name] = data.decode()
        if edits[name].count(old) != 1:
            raise ValueError(f'hook anchor missing or ambiguous: {name}')
        edits[name] = edits[name].replace(old, new, 1)

    for role in ('client', 'server'):
        directory = f'userspace/apps/ipctest-{role}'
        gpr = f'{directory}/ipctest_{role}.gpr'
        dirs = '".", "build/generated"' if role == 'client' else '"."'
        replace(gpr, f'for Source_Dirs use ({dirs});',
                f'for Source_Dirs use ({dirs}, "../../lib/display", '
                '"../../services/intel-gpu", "../../mesa/anv", '
                '"../../../tests/mesa-anv/native-integration");')
        replace(gpr, '"-O2",', '"-O2", "-gnat2022",')
        replace(f'{directory}/main.adb', 'with Interfaces; use Interfaces;',
                'with GPU_Admission_Probe;\nwith Interfaces; use Interfaces;')
    replace('userspace/apps/ipctest-client/main.adb',
            '   debugPrint ("ipctest-client: starting" & LF);',
            '   debugPrint ("ipctest-client: starting" & LF);\n'
            '   GPU_Admission_Probe.Client (CAP_SLOT_IPCTEST);')
    replace('userspace/apps/ipctest-server/main.adb',
            '      if msg.tag.label = OP_FAIR_BEGIN then',
            '      if msg.tag.label = 16#0A21# then\n'
            '         GPU_Admission_Probe.Server (from, msg);\n'
            '      elsif msg.tag.label = OP_FAIR_BEGIN then')
    # Validate every anchor before changing any file. These are test-app hooks;
    # production bootstrap, driver, kernel, manifests and runtime stay intact.
    results = []
    for name, text in edits.items():
        (workspace / name).write_text(text)
        results.append({'path': name, 'before': inputs[name]['sha256'],
                        'after': hashlib.sha256(text.encode()).hexdigest()})
    receipt.write_text(json.dumps({'scope': 'negative-admission-only',
                                  'edits': results}, indent=2) + '\n')
    print(receipt)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('workspace', type=Path)
    prepare(parser.parse_args().workspace)
