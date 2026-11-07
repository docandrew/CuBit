#!/usr/bin/env python3
"""Pinned x86-64 codegen checks, not an allocator latency benchmark."""
import re
import subprocess
from pathlib import Path

obj = "kernel/build/bootallocator.o"


def disassemble(symbol):
    output = subprocess.check_output(
        ["objdump", "-dr", f"--disassemble={symbol}", obj], text=True
    )
    if f"<{symbol}>:" not in output:
        raise SystemExit(f"FAIL: missing {symbol}")
    return output


symbols = subprocess.check_output(["nm", "-S", obj], text=True)
state = re.search(r"^\w+\s+(\w+)\s+\w\s+bootallocator__reservations$", symbols, re.M)
if not state:
    raise SystemExit("FAIL: missing reservation state")
# Pinned representation: packed frame bits for the 1 GiB bootstrap window
# (2**18 frames) followed by a 32-bit high-water PFN.
state_bytes = int(state[1], 16)
if state_bytes != 32772:
    raise SystemExit("FAIL: review changed boot bitmap/high-water representation")
bitmap_bytes = state_bytes - 4
last_frame = bitmap_bytes * 8 - 1
query = disassemble("bootallocator__isfree")
reserve = disassemble("bootallocator__frames__reserve")
highest = disassemble("bootallocator__highestpfnallocated")
for name, output in (("isFree", query), ("Reserve", reserve)):
    if not re.search(rf"\bcmp\s+\$0x{last_frame:x},", output):
        raise SystemExit(f"FAIL: {name} inclusive bound disagrees with bitmap")
    if not re.search(r"\bsar\s+\$0x3,", output) or not re.search(r"\band\s+\$0x7,", output):
        raise SystemExit(f"FAIL: {name} no longer uses direct packed-bit indexing")
if not re.search(rf"0x{bitmap_bytes:x}\(%rdi\)", reserve):
    raise SystemExit("FAIL: high-water write not immediately after packed bitmap")
# RIP-relative PC32 relocation: field offset minus the 4-byte displacement.
if f"bootallocator__reservations+0x{bitmap_bytes - 4:x}" not in highest:
    raise SystemExit("FAIL: high-water reader does not match the reservation field")
if re.search(r"\b(call\w*|i?div\w*|push\w*|rep\w*)\s", reserve):
    raise SystemExit("FAIL: reservation gained calls, division, stack saves or a bulk copy")
if re.search(r"\bsub\s+.*%rsp", reserve):
    raise SystemExit("FAIL: reservation gained a stack allocation")
usage = Path("kernel/build/bootallocator.su").read_text()
if not re.search(r":Reserve\s+8\s+static$", usage, re.M):
    raise SystemExit("FAIL: reservation no longer has the minimal leaf stack usage")
if re.search(r"(?i)(reservation_properties|__postconditions|__assert|__prove_)", symbols):
    raise SystemExit("FAIL: proof/check symbols entered production allocator")
# Retirement (tests/boot-reclaim): the bitmap lies alone in the reclaimable
# boot section, and every public entry reads the phase before the bitmap, so
# nothing reads or reserves once the section belongs to the buddy allocator.
table = subprocess.check_output(["objdump", "-t", obj], text=True)
placed = re.search(r"^\w+\s+\w\s+\w*\s+(\S+)\s+\w+\s+bootallocator__reservations$", table, re.M)
if not placed or placed[1] != ".bss.boot_reclaim":
    raise SystemExit("FAIL: boot bitmap is not in the reclaimable boot section")
for entry in ("bootallocator__isfree", "bootallocator__allocframes",
              "bootallocator__highestpfnallocated", "bootallocator__numfreeframes"):
    body = disassemble(entry)
    phase = body.find("bootallocator__currentphase")
    bitmap = min((at for at in (body.find("bootallocator__reservations"),
                                body.find("bootallocator__frames__")) if at >= 0),
                 default=-1)
    if phase < 0 or (bitmap >= 0 and bitmap < phase):
        raise SystemExit(f"FAIL: {entry} touches the bitmap before checking the phase")
    if "bootallocator__retiredexception" not in body and "__gnat_rcheck" not in body \
       and "__gnat_raise" not in body and "last_chance" not in body.lower():
        raise SystemExit(f"FAIL: {entry} has no refusal path after retirement")
kernel = Path("kernel/cubit_kernel")
if kernel.exists():
    linked = subprocess.check_output(["nm", "-S", str(kernel)], text=True)
    def symbol(name):
        found = re.search(rf"^(\w+)\s+(?:(\w+)\s+)?\w\s+{name}$", linked, re.M)
        if not found:
            raise SystemExit(f"FAIL: kernel lacks {name}")
        return int(found[1], 16), int(found[2] or "0", 16)
    first, _ = symbol("sboot_reclaim")
    limit, _ = symbol("eboot_reclaim")
    where, size = symbol("bootallocator__reservations")
    if first % 4096 or limit % 4096 or not (first <= where and where + size <= limit):
        raise SystemExit("FAIL: reclaimable boot section is not whole pages around the bitmap")
    if limit - first != (size + 4095) // 4096 * 4096:
        raise SystemExit("FAIL: reclaimable boot section holds more than the boot bitmap")
    print(f"PASS boot reclaim section: {limit - first} bytes, bitmap only")
print(f"PASS boot reservation codegen: {bitmap_bytes}-byte packed bitmap; "
      f"inclusive PFN {last_frame}; leaf reservation, no calls/division/copies/proof code")
