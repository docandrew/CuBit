#!/usr/bin/env python3
"""Check ACPI launch evidence without ordering post-operation narration."""

import argparse
import re
import sys
from pathlib import Path


class EvidenceError(ValueError):
    pass


def need(condition, message):
    if not condition:
        raise EvidenceError(message)


def pci_config(text):
    read = re.search(r"^acpi-pci-check: host bridge 00:00\.0 ([0-9A-F]{4}):([0-9A-F]{4})$", text, re.M)
    need(read, "no host bridge IDs read through the window")
    ids = (read.group(1), read.group(2))
    # Kernel loader text can precede or interrupt a complete routed log body.
    # Match the specific producer/body, rather than assuming column zero.
    enumerated = re.findall(r"\bdevmgr: match 00:00\.0 ([0-9A-F]{4}):([0-9A-F]{4}) class", text)
    enumerated += re.findall(r"\bdevmgr: PCI 00:00\.0 vendor=([0-9A-F]{4}) device=([0-9A-F]{4}) class=", text)
    need(enumerated, "devmgr did not report 00:00.0")
    need(set(enumerated) == {ids}, "window IDs disagree with devmgr enumeration")
    marker = "acpi-pci-check: attempting forbidden write at 0000600000000000"
    need(text.count(marker) == 1, "missing or repeated forbidden-write attempt")
    before, after = text.split(marker)
    loaded = {int(pid) for pid in re.findall(
        r"Loaded module acpi-pci-check\.a\S* w/ process ID\s+(\d+)\b", before)}
    # The numeric loader PID can itself be torn ("3" + routed text + "2").
    # Its complete named kernel fault record gives an independent identity.
    # KERN-003's ABI uses 24 slot bits (CuBit.Process_IDs.Slot_Bits).
    identities = [int(word, 16) for word in re.findall(
        r"\blogstore: kernel: process killed #\d+ name=acpi-pci-check\.a\S* "
        r"pid=0x([0-9A-Fa-f]+) kind=write-protection vector=14\b", after)]
    need(all(0 < word < 2 ** 64 and word & (2 ** 24 - 1) for word in identities),
         "invalid named fault identity")
    named = {word & (2 ** 24 - 1) for word in identities}
    need(len(loaded) <= 1 and len(named) <= 1, "ambiguous checker process identity")
    need(loaded or named, "no intact loader or named fault identity for acpi-pci-check.app")
    need(not loaded or not named or loaded == named, "loader and named fault identities disagree")
    pid = next(iter(loaded or named))
    need(re.search(rf"USER-MEMORY-FAULT: pid\s+{pid}\s+address\s+{0x600000000000}"
                   r"\s+kind=write-protection\b", after),
         "the checker write to the read-only mapping did not fault")
    need("forbidden write was not stopped" not in text, "the write was not stopped")
    return f"host bridge {':'.join(ids)} agrees with devmgr; checker write faulted (slot {pid})"


def gpio_launch(text):
    held = list(re.finditer(r"\bprocmgr: held PID (\d+) for its supervisor", text))
    admitted = list(re.finditer(
        r"gpio-launch-check: sink admission ok status=COMPLETED badge=0x([0-9A-Fa-f]+) "
        r"owner=0x([0-9A-Fa-f]+)", text))
    released = list(re.finditer(r"\bprocmgr: held pid(\d+) released by devmgr", text))
    ran = list(re.finditer(r"hid-launch-check: parameters ", text))
    need(len(held) == len(admitted) == len(released) == len(ran) == 1,
         "missing or repeated held/admitted/released/HID-start evidence")
    held, admitted, released, ran = held[0], admitted[0], released[0], ran[0]
    need(int(held.group(1)) == int(released.group(1)) == int(admitted.group(2), 16),
         "held/released child and admitted sink owner disagree")
    need(held.start() < admitted.start() < ran.start(), "HID ran before its sink admission")
    # procmgr queues its release diagnostic before resume, but its routed
    # log can be mirrored after the child's direct output. Both must follow
    # sink admission; their printed order is not the operation order.
    need(admitted.start() < released.start(), "release was reported before sink admission")
    return "held HID child owns its admitted GPIO sink before its first output; release confirmed"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("kind", choices=["pci-config", "gpio-launch"])
    parser.add_argument("serial", type=Path)
    args = parser.parse_args()
    try:
        text = args.serial.read_text(errors="replace").replace("\r", "")
        result = (pci_config if args.kind == "pci-config" else gpio_launch)(text)
    except (EvidenceError, OSError) as error:
        print(f"{args.kind}: {error}", file=sys.stderr)
        return 1
    print("PASS: " + result)
    return 0


if __name__ == "__main__":
    sys.exit(main())
