#!/usr/bin/env python3
"""Checks an xhci-bringup headless serial log (tests/headless/run.sh).

The IdeaPad's boot stall (images 31-33): devmgr configured its two xHCI
controllers one after the other with a synchronous call that each driver
answered only after its whole USB bring-up (firmware handoff, reset, a
fixed 3 s settle, enumeration, the storage probe), so devmgr and every
driver calling it waited ~10 s.

QEMU here has the same shape: two qemu-xhci controllers, the first with
nothing attached (as 00:0d.0) and the second with a boot mouse, a boot
keyboard and a USB storage disk holding an ISO9660 image (as 00:14.0 with
the live stick). The log must show:

* devmgr's configure call answered within ACCEPT_LIMIT_MS for each one
  (devmgr never waits on a controller's bring-up);
* each controller's bring-up reported within BRING_UP_LIMIT_MS (no fixed
  settle: the window ends once the ports hold still);
* devmgr's firmware handoff of each controller, halted, before its first
  DMA grant; with --iommu, each passes DMA through from boot until that
  grant and then has its own domain, with no DMA fault;
* devmgr's boot wait for the reports ended with the last one, within
  WAIT_MARGIN_MS of the slowest bring-up, and never at its 10 s ceiling;
* the right devices on each, and the storage disk's apps/ tree mounted;
* no STUCK-CALL report involving devmgr and xhci.drv.
"""
import re
import sys

LOCATION = r'[0-9A-F]{2}:[0-9A-F]{2}\.[0-7]'
# devmgr's own wait on one controller: a message check and a reply.
ACCEPT_LIMIT_MS = 250
# A controller's bring-up under KVM: reset, the quiet settle window
# (XHCI_Boot_Settle.Quiet_Ms, 300 ms), port resets, enumeration and the
# storage probe. The fixed 3 s window alone exceeded it.
BRING_UP_LIMIT_MS = 1500
# devmgr's boot wait for the reports (Await_XHCI_Bring_Ups) past the slowest
# bring-up: taking the last report and attaching its storage. It must end
# with the reports, far before its 10 s ceiling (XHCI_Bring_Up_Wait_Ms).
WAIT_MARGIN_MS = 250


def fail(message):
    print(f'xhci-bringup: FAIL {message}', file=sys.stderr)
    sys.exit(1)


def main(path):
    text = open(path, errors='replace').read()

    locations = sorted(set(re.findall(
        rf'devmgr: match ({LOCATION}) 1B36:000D class 0C\.03\.30 [^\n]*-> xhci\.drv', text)))
    if len(locations) != 2:
        fail(f'expected two xHCI controllers matched, found {locations}')

    accepted = {}
    for location, ms in re.findall(
            rf'devmgr: xhci\.drv ({LOCATION}) configure answered in (\d+) ms', text):
        accepted[location] = int(ms)
    started = {}
    for location, kbd, mouse, storage, ms in re.findall(
            rf'devmgr: xhci\.drv ({LOCATION}) started; root ports \d+, connected \d+; '
            r'devices keyboard=(\d) mouse=(\d) storage=(\d); bring-up (\d+) ms', text):
        started[location] = ((kbd, mouse, storage), int(ms))

    for location in locations:
        if location not in accepted:
            fail(f'{location}: no "configure answered" line')
        if accepted[location] > ACCEPT_LIMIT_MS:
            fail(f'{location}: devmgr waited {accepted[location]} ms on the configure call '
                 f'(limit {ACCEPT_LIMIT_MS} ms)')
        if location not in started:
            fail(f'{location}: no "started ... bring-up N ms" line')
        if started[location][1] > BRING_UP_LIMIT_MS:
            fail(f'{location}: bring-up took {started[location][1]} ms '
                 f'(limit {BRING_UP_LIMIT_MS} ms)')

    kinds = sorted(started[location][0] for location in locations)
    if kinds != [('0', '0', '0'), ('1', '1', '1')]:
        fail(f'expected one empty controller and one with keyboard, mouse and storage: {started}')

    # Linux's early-handoff order (devmgr Early_XHCI_Handoff, the kernel's
    # Handoff_Contexts): devmgr takes each controller from the firmware and
    # halts it before its first DMA grant. qemu-xhci has no legacy
    # capability. With an IOMMU (--iommu), each controller passes DMA
    # through from boot until that grant, and only then has its own domain.
    for location in locations:
        handoff = re.search(rf'devmgr: xhci\.drv {location} firmware handoff (legacy=[a-z-]+)[^\n]*halted in \d+ ms',
                            text)
        if not handoff:
            fail(f'{location}: no "firmware handoff ... halted" line before its DMA grant')
        if handoff.group(1) != 'legacy=absent':
            fail(f'{location}: qemu-xhci has no legacy capability, devmgr saw {handoff.group(1)}')
    if re.search(r'IOMMU: unit\d+ at 0x[0-9A-F]+: translating', text):
        for location in locations:
            low = location.lower()
            passing = text.find(f'IOMMU: xHCI {low} passes DMA through until its firmware handoff')
            leaving = text.find(f'IOMMU: xHCI {low} leaves pass-through for its own domain')
            if passing < 0 or leaving < 0:
                fail(f'{location}: the IOMMU did not pass it through until its handoff, '
                     f'then give it its own domain')
            if leaving < passing:
                fail(f'{location}: its own domain came before its pass-through')
            if text.count(f'IOMMU: xHCI {low} passes DMA through') != 1:
                fail(f'{location}: passed through more than once (Handoff_Contexts never reopens)')
        if 'kernel: DMA fault' in text:
            fail('a DMA fault with the IOMMU translating')

    # devmgr's boot wait ends with the last report, not at its ceiling: a
    # taken report counts at once.
    if 'xHCI bring-up was not reported within' in text:
        fail('devmgr waited out its bring-up ceiling (a report did not end the wait)')
    waits = [int(ms) for ms in re.findall(
        r'devmgr: xHCI bring-ups reported; devmgr waited (\d+) ms', text)]
    if len(waits) != 1:
        fail(f'expected one "xHCI bring-ups reported" line, found {len(waits)}')
    slowest = max(ms for _, ms in started.values())
    if waits[0] > slowest + WAIT_MARGIN_MS:
        fail(f'devmgr waited {waits[0]} ms for bring-ups that took at most {slowest} ms '
             f'(margin {WAIT_MARGIN_MS} ms)')

    # The storage disk's ISO9660 apps/ tree mounts through that controller.
    if 'FS: native USB ISO9660 apps mounted' not in text:
        fail('the USB storage disk was not mounted')

    stuck = re.findall(r'STUCK-CALL: [^\n]*(?:devmgr\.svc[^\n]*xhci\.drv|xhci\.drv[^\n]*devmgr\.svc)'
                       r'[^\n]*', text)
    # A child's introduction thread waiting for logstore is a kept call
    # (CuBit.Late_Introduction): devmgr answers it once procmgr offers
    # logstore. Only devmgr waiting on xhci, or a queued xhci call (devmgr
    # not receiving), is the stall.
    stuck = [line for line in stuck if 'devmgr.svc(' in line.split('->')[0] or ' queued ' in line]
    if stuck:
        fail('devmgr and xhci.drv stuck: ' + '; '.join(stuck))

    for marker in ('EXCEPTION', 'PANIC', 'User-space exception'):
        if marker in text:
            fail(f'fault marker {marker!r} in the log')
    print(f'xhci-bringup: PASS devmgr waited {waits[0]} ms; ' + ', '.join(
        f'{location} accepted {accepted[location]} ms bring-up {started[location][1]} ms'
        for location in locations))


if __name__ == '__main__':
    main(sys.argv[1])
