#!/usr/bin/env bash
# filesystem.svc keeps its large buffers in service-owned storage, never in
# a stack frame (FS-022 follow-up). The loader backs every page of the image
# when the service starts, but stack pages are faulted in on first touch:
# a frame of hundreds of KiB (appendRun's 512 KiB write staging, the
# commit's) was first touched mid-request, and on a full machine that fault
# killed the service (fs-coherence readdir-after-other-create, 128 MiB).
#
# Reads the linked service (after make -C kernel filesystem or world) and
# fails if any function sets up a frame above LARGEST_FRAME_BYTES.
#   tests/filesystem-stack-frames/check.sh [path/to/filesystem.svc]
set -uo pipefail
root=$(cd "$(dirname "$0")/../.." && pwd)
service=${1:-$root/userspace/services/filesystem/build/filesystem.svc}
# Twice the largest frame of plain locals the service has (appendRun's,
# about 31 KiB); a buffer this size belongs in storage the service owns.
LARGEST_FRAME_BYTES=$((64 * 1024))

if [ ! -f "$service" ]; then
    echo "FS-STACK-FRAMES: FAIL no $service (build it first)"
    exit 1
fi
objdump -d --no-show-raw-insn "$service" | python3 -I -c '
import re, sys
limit = int(sys.argv[1])
symbol = re.compile(r"^[0-9a-f]+ <(.+)>:$")
# A frame is set up by "sub $N,%rsp", or by a probing loop that first
# computes its end in %r11 (-fstack-clash-protection).
frame = re.compile(r"\s(?:sub\s+\$0x([0-9a-f]+),%rsp|lea\s+-0x([0-9a-f]+)\(%rsp\),%r11)$")
largest = {}
function = None
for line in sys.stdin:
    m = symbol.match(line)
    if m:
        function = m.group(1)
        continue
    m = frame.search(line.rstrip())
    if m and function:
        size = int(m.group(1) or m.group(2), 16)
        if size < 2 ** 31:   # "sub $-128" is an add
            largest[function] = max(largest.get(function, 0), size)
over = sorted(((s, f) for f, s in largest.items() if s > limit), reverse=True)
for size, function in over:
    print("FS-STACK-FRAMES: %s sets up a %d-byte frame (limit %d)" % (function, size, limit))
if not largest:
    print("FS-STACK-FRAMES: FAIL no frames found (not a disassembly?)")
    sys.exit(1)
if over:
    print("FS-STACK-FRAMES: FAIL")
    sys.exit(1)
size, function = max((s, f) for f, s in largest.items())
print("FS-STACK-FRAMES: largest frame %d bytes (%s)" % (size, function))
print("FS-STACK-FRAMES: PASS")
' "$LARGEST_FRAME_BYTES"
