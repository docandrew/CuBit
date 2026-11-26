"""Require exact user fault evidence for each independently launched probe."""
import re
import sys
from pathlib import Path

log = Path(sys.argv[1]).read_text(errors="replace")
if "PROTECTION-FAULT: FAIL" in log:
    raise SystemExit("protected access unexpectedly succeeded")
# MEM-007 (K12): a discarded read-only page still refuses a write; a
# discarded reserved page still faults on a read.
KINDS = {"guard-read": "unmapped", "readonly-write": "write-protection",
         "discard-readonly-write": "write-protection", "discard-reserved-read": "unmapped"}
for kind, category in KINDS.items():
    armed = list(re.finditer(rf"PROTECTION-FAULT: {kind} armed pid=(\d+) address=([0-9a-f]+)", log))
    if len(armed) != 1:
        raise SystemExit(f"{kind}: expected exactly one armed probe")
    posix_pid, address = armed[0].groups()
    # getpid() is the identity folded to a pid_t (KERN-003); its low 24 bits
    # are the kernel slot its serial diagnostics print.
    pid = str(int(posix_pid) & 0xFFFFFF)
    # The probe's line is a record logstore mirrors (ABI-002): it reaches
    # serial when logstore drains it, often after the kernel's own fault
    # line. Lines of two sources are not ordered; pid and address are the
    # match (one armed probe of each kind).
    fault = re.compile(rf"USER-MEMORY-FAULT: pid {pid} address {int(address, 16)} kind={category}\b").search(log)
    if not fault:
        raise SystemExit(f"{kind}: missing matching address/PID fault")
    if not re.compile(rf"Process.reclaimProcess: stopped PID {pid}\b").search(log, fault.end()):
        raise SystemExit(f"{kind}: missing process retirement")
print("Protection faults: PASS guard read, read-only write, discarded read-only write and "
      "discarded reserved read trapped")
