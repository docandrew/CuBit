#!/usr/bin/env python3
"""IOMMU-001 serial-log checks (tests/headless/run.sh).

  check-iommu.py active   SERIAL   QEMU with intel-iommu: the IOMMU is active,
                                   NVMe and virtio-net each got their own
                                   domain, and no DMA fault was logged.
  check-iommu.py fault    SERIAL   the generic-check fault probe: the DMA
                                   round trip through edu's domain matched,
                                   and its read from outside its block gave
                                   exactly one typed DMA fault for edu's
                                   requester (reason not-mapped), and the
                                   system kept running.
  check-iommu.py degraded SERIAL   no IOMMU: the typed degraded record says
                                   DMA is unconfined (no DMAR table).
"""
import re
import sys

ACTIVE = re.compile(r"kernel: IOMMU active, units (\d+) of (\d+) translating")
DEGRADED = re.compile(r"kernel: IOMMU degraded: DMA unconfined \(([^)]*)\)")
DOMAIN = re.compile(r"IOMMU: domain\s+(\d+) for requester ([0-9a-f]{2}:[0-9a-f]{2}\.[0-7]) on unit\s+(\d+)")
CONSOLE_FAULT = re.compile(
    r"^kernel: DMA fault requester ([0-9a-f]{2}:[0-9a-f]{2}\.[0-7]) (read|write) (0x[0-9A-Fa-f]+) "
    r"class (\S+) reason (\S+) \((0x[0-9A-Fa-f]+)\) unit (0x[0-9A-Fa-f]+)", re.M)
# logstore's typed record, as logstore echoes it on its first line.
STORED_FAULT = re.compile(r"^kernel: DMA fault #(\d+) requester ([0-9a-f]{2}:[0-9a-f]{2}\.[0-7]) ", re.M)
PCI = re.compile(r"devmgr: PCI ([0-9A-Fa-f]{2}):([0-9A-Fa-f]{2})\.([0-7]) vendor=(0x)?([0-9A-Fa-f]{4}) device=(0x)?([0-9A-Fa-f]{4})")


def fail(message):
    print(f"check-iommu: FAIL: {message}")
    sys.exit(1)


def functions(text):
    """vendor:device -> [bb:dd.f] from devmgr's enumeration lines."""
    found = {}
    for m in PCI.finditer(text):
        location = f"{m.group(1).lower()}:{m.group(2).lower()}.{m.group(3)}"
        key = f"{m.group(5).upper()}:{m.group(7).upper()}"
        found.setdefault(key, []).append(location)
    return found


def check_active(text):
    m = ACTIVE.search(text)
    if not m:
        fail("no 'kernel: IOMMU active' record")
    if m.group(1) != m.group(2):
        fail(f"only {m.group(1)} of {m.group(2)} units translating")
    domains = {d.group(2): int(d.group(1)) for d in DOMAIN.finditer(text)}
    pci = functions(text)
    wanted = {"1B36:0010": "NVMe", "1AF4:1041": "virtio-net (modern)"}
    for key, name in wanted.items():
        locations = pci.get(key, [])
        if not locations:
            fail(f"{name} ({key}) not enumerated")
        for location in locations:
            if location not in domains:
                fail(f"{name} at {location} has no IOMMU domain")
            print(f"check-iommu: {name} at {location}: domain {domains[location]}")
    if len(set(domains.values())) != len(domains):
        fail("two requesters share a domain")
    if CONSOLE_FAULT.search(text) or STORED_FAULT.search(text):
        fail("a DMA fault was logged: " + (CONSOLE_FAULT.search(text) or STORED_FAULT.search(text)).group(0))
    print(f"check-iommu: PASS active ({m.group(1)} unit(s)), {len(domains)} domain(s), no DMA fault")


def check_fault(text):
    if not ACTIVE.search(text):
        fail("no 'kernel: IOMMU active' record")
    edu = functions(text).get("1234:11E8", [])
    if len(edu) != 1:
        fail(f"expected one edu device, found {edu}")
    edu = edu[0]
    domains = {d.group(2): int(d.group(1)) for d in DOMAIN.finditer(text)}
    if edu not in domains:
        fail(f"edu at {edu} has no IOMMU domain")
    if not re.search(r"generic-check: stage dma-round-trip ok bytes=\s*\d+ match", text):
        fail("the DMA round trip through edu's domain did not match")
    probe = re.search(r"generic-check: stage dma-unmapped ok probe=0x([0-9A-F]+)", text)
    if not probe:
        fail("the unmapped-DMA probe did not run")
    probe_address = int(probe.group(1), 16)
    faults = list(CONSOLE_FAULT.finditer(text))
    if len(faults) != 1:
        fail(f"expected exactly one DMA fault record, found {len(faults)}: "
             + "; ".join(f.group(0) for f in faults))
    f = faults[0]
    if f.group(1) != edu:
        fail(f"the fault names {f.group(1)}, not edu at {edu}")
    if f.group(2) != "read":
        fail("the fault is not a read")
    if int(f.group(3), 16) != probe_address:
        fail(f"the fault address {f.group(3)} is not the probe {probe_address:#x}")
    if f.group(5) != "not-mapped":
        fail(f"the fault reason is {f.group(5)}, not not-mapped")
    stored = list(STORED_FAULT.finditer(text))
    if len(stored) != 1 or stored[0].group(1) != "1" or stored[0].group(2) != edu:
        fail("logstore did not keep exactly one typed DMA fault record #1 for edu: "
             + "; ".join(s.group(0) for s in stored))
    # Still running afterwards: the probe precedes the interrupt stage and
    # boot-logs reads logstore later.
    after = text[f.end():]
    for marker in ("generic-check: bring-up complete", "boot-logs: generic-check: bring-up complete"):
        if marker not in after:
            fail(f"'{marker}' did not follow the fault")
    print(f"check-iommu: PASS fault: one typed DMA fault for edu {edu} at {probe_address:#x} "
          f"(class {f.group(4)}, reason {f.group(5)}), domain {domains[edu]}, system running")


def check_degraded(text):
    m = DEGRADED.search(text)
    if not m:
        fail("no typed 'kernel: IOMMU degraded: DMA unconfined' record")
    if m.group(1) != "no DMAR table":
        fail(f"degraded for '{m.group(1)}', expected 'no DMAR table'")
    if ACTIVE.search(text):
        fail("both active and degraded")
    print(f"check-iommu: PASS degraded ({m.group(1)})")


def main():
    if len(sys.argv) != 3 or sys.argv[1] not in ("active", "fault", "degraded"):
        print(__doc__)
        sys.exit(2)
    with open(sys.argv[2], encoding="utf-8", errors="replace") as log:
        text = log.read()
    {"active": check_active, "fault": check_fault, "degraded": check_degraded}[sys.argv[1]](text)


if __name__ == "__main__":
    main()
