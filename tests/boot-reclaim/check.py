#!/usr/bin/env python3
"""Boot memory reclaim, read from a headless serial log.

Every boot: the boot allocator is retired and its reclaimable section (its
bitmap, 32 KiB and more) goes to the buddy allocator, whose managed total
grows by exactly that; the loader metadata follows the same way.

boot-large-module: the synthetic module nobody consumes comes back whole,
the managed total growing by its size.

storage-grants (a writable seed in the boot image, a RAM block driver):
the driver releases its view and devmgr releases the seed's pages.

Before this change none of these lines exists: the boot arena (kernel to the
end of the last module) and the bitmap were never returned.
"""
import re
import sys

KIB = 1024
BITMAP_KIB = 32  # one bit per frame of the 1 GiB bootstrap window
LARGE_MODULE_MIB = 340  # tests/headless/run.sh LARGE_MODULE_MIB

log_path, test = sys.argv[1], sys.argv[2]
text = open(log_path, errors="replace").read()
failures = []


def total_step(pattern, label):
    match = re.search(pattern, text)
    if not match:
        failures.append(f"missing: {label}")
        return None
    returned, before, after = (int(group) for group in match.groups())
    print(f"boot-reclaim: {label}: {returned} KiB, buddy total {before} -> {after} KiB")
    if after - before != returned:
        failures.append(f"{label}: total grew by {after - before} KiB, not {returned}")
    return returned


section = total_step(
    r"Boot reclaim: boot allocator retired, section returned (\d+) KiB "
    r"\(buddy total (\d+) -> (\d+) KiB\)", "boot allocator section")
if section is not None and section < BITMAP_KIB:
    failures.append(f"section {section} KiB is smaller than the {BITMAP_KIB} KiB bitmap")
total_step(
    r"Boot reclaim: loader metadata returned (\d+) KiB "
    r"\(buddy total (\d+) -> (\d+) KiB\)", "loader metadata")

if test == "boot-large-module":
    match = re.search(
        r"Modules: released unconsumed module large-module\.img: (\d+) KiB "
        r"\(buddy total (\d+) KiB\)", text)
    if not match:
        failures.append("the unconsumed large module was not released")
    else:
        returned, after = int(match[1]), int(match[2])
        print(f"boot-reclaim: large module: {returned} KiB back, "
              f"buddy total {after - returned} -> {after} KiB")
        if returned != LARGE_MODULE_MIB * KIB:
            failures.append(f"large module returned {returned} KiB, "
                            f"not {LARGE_MODULE_MIB * KIB}")

if test == "storage-grants":
    if "ramdisk: boot image view released" not in text:
        failures.append("missing: ramdisk: boot image view released")
    match = re.search(r"devmgr: boot image seed released \(pages (\d+); "
                      r"managed memory (\d+) -> (\d+) KiB\)", text)
    if not match:
        failures.append("missing: devmgr: boot image seed released")
    else:
        pages, before, after = (int(group) for group in match.groups())
        print(f"boot-reclaim: writable seed: {pages} pages back, "
              f"managed memory {before} -> {after} KiB")
        if pages == 0 or after - before != pages * 4:
            failures.append(f"seed: managed memory grew by {after - before} KiB, "
                            f"not {pages * 4}")
    for refusal in ("boot image view not released",
                    "boot image seed pages not released",
                    "MAP_INTO: boot image pages released or unavailable"):
        if refusal in text:
            failures.append(f"refused: {refusal}")

if failures:
    for failure in failures:
        print(f"boot-reclaim: FAIL: {failure}", file=sys.stderr)
    sys.exit(1)
print("boot-reclaim: PASS")
