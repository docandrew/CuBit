#!/usr/bin/env python3
"""Checks a multi-instance headless serial log (tests/headless/run.sh).

QEMU had two qemu-xhci controllers (1B36:000D), one with a USB keyboard
and one with a USB optical drive, and two NVMe controllers. The log must
show, for each controller, devmgr's match line with its interrupt
capabilities, one driver instance started for it, and that instance
enumerating its own devices (named by the location devmgr gave it); and
both NVMe disks attached as @nvme:0 and @nvme:1.
"""
import re
import sys

LOCATION = r'[0-9A-F]{2}:[0-9A-F]{2}\.[0-7]'


def fail(message):
    print(f'multi-instance: FAIL {message}', file=sys.stderr)
    sys.exit(1)


def main(path):
    text = open(path, errors='replace').read()

    # devmgr's match lines: QEMU's qemu-xhci has an MSI-X table.
    matches = re.findall(
        rf'devmgr: match ({LOCATION}) 1B36:000D class 0C\.03\.30 '
        r'msi=(\S+) msix=(\S+) -> xhci\.drv', text)
    xhci_locations = sorted({m[0] for m in matches})
    if len(xhci_locations) != 2:
        fail(f'expected two xHCI match lines, found {matches}')
    for location, msi, msix in matches:
        if not re.fullmatch(r'1/\d+', msix):
            fail(f'xHCI {location} match line shows no MSI-X: msix={msix}')
    if re.search(r'devmgr: match [^\n]*xhci\.drv[^\n]*not started', text):
        fail('an xHCI controller was not started')

    # Every match line carries the interrupt fields.
    for line in re.findall(r'devmgr: match [0-9A-F]{2}:[^\n]*', text):
        if not re.search(r' msi=(0|1/\d+) msix=(0|1/\d+)( caps=\w+)? -> ', line):
            fail(f'match line without interrupt capabilities: {line}')

    # One instance started per controller, each reporting only its own
    # devices (xhci's configure reply, logged by devmgr with the location).
    devices = {}
    for location in xhci_locations:
        found = re.findall(
            rf'devmgr: xhci\.drv {location} started; root ports \d+, connected \d+; '
            r'devices keyboard=(\d) mouse=(\d) storage=(\d)', text)
        if not found:
            fail(f'no xhci.drv instance started for {location}')
        devices[location] = found[0]
    kinds = sorted(devices.values())
    if kinds != [('0', '0', '1'), ('1', '0', '0')]:
        fail(f'expected one keyboard-only and one storage-only controller: {devices}')

    # The storage controller's optical drive serves @cd:0: its disc's
    # apps/ tree mounts (the filesystem searches it for unqualified names).
    if 'FS: native USB ISO9660 apps mounted' not in text:
        fail('the optical disc on the storage controller was not mounted')

    # Both NVMe controllers: their own instance, attached by ordinal.
    # (boot-logs repeats devmgr's lines: count each once.)
    nvme = sorted(set(re.findall(rf'devmgr: nvme\.drv ({LOCATION}) attached as @nvme:(\d)', text)))
    if sorted(n[1] for n in nvme) != ['0', '1'] or len({n[0] for n in nvme}) != 2:
        fail(f'expected @nvme:0 and @nvme:1 on two controllers: {nvme}')
    controllers = re.findall(rf'NVMe Driver: controller ({LOCATION})', text)
    if sorted(set(controllers)) != sorted({n[0] for n in nvme}):
        fail(f'NVMe instances named {controllers}, devmgr attached {nvme}')

    # devmgr's lines reached logstore (boot-logs prints what it keeps).
    for location in xhci_locations:
        if not re.search(rf'boot-logs: devmgr: xhci\.drv {location} started', text):
            fail(f'devmgr line for {location} did not reach logstore')
    print(f'multi-instance: PASS xhci {devices} nvme {sorted(nvme)}')


if __name__ == '__main__':
    main(sys.argv[1])
