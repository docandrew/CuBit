#!/usr/bin/env python3
"""The console's process states (CCL.Interfaces.Processes.Kernel_Run_State)
must track the kernel's ProcessState (kernel/src/process.ads) after INVALID,
one for one. A kernel state the console lacks made :ps silently drop every
process in it (vmm.app in VMHALTED, 2026-11-21)."""
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]


def kernel_states():
    text = (ROOT / 'kernel/src/process.ads').read_text()
    body = re.search(r'type ProcessState is \((.*?)\);', text, re.S).group(1)
    names = [re.sub(r'--.*', '', line).strip().rstrip(',') for line in body.splitlines()]
    names = [n for n in names if n]
    assert names[0] == 'INVALID', names[0]
    return names[1:]


def console_states():
    text = (ROOT / 'userspace/ccl/src/ccl-interfaces-processes.ads').read_text()
    body = re.search(r'type Run_State is\s*\((.*?)\);', text, re.S).group(1)
    names = [n.strip() for n in body.replace('\n', ' ').split(',') if n.strip()]
    assert names[-1] == 'Unknown', names[-1]
    return names[:-1]


def main():
    kernel, console = kernel_states(), console_states()
    if len(kernel) != len(console):
        print(f'FAIL: the kernel has {len(kernel)} process states after INVALID, '
              f'the console {len(console)}: kernel {kernel}, console {console}')
        return 1
    print(f'PASS: {len(console)} console process states track the kernel\'s')
    return 0


sys.exit(main())
