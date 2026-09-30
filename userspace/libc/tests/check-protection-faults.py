"""Require exact user fault evidence for each independently launched probe."""
import re
import sys
from pathlib import Path

log = Path(sys.argv[1]).read_text(errors="replace")
if "PROTECTION-FAULT: FAIL" in log:
    raise SystemExit("protected access unexpectedly succeeded")
for kind in ("guard-read", "readonly-write"):
    armed = list(re.finditer(rf"PROTECTION-FAULT: {kind} armed pid=(\d+) address=([0-9a-f]+)", log))
    if len(armed) != 1:
        raise SystemExit(f"{kind}: expected exactly one armed probe")
    pid, address = armed[0].groups()
    category = "unmapped" if kind == "guard-read" else "write-protection"
    fault = re.compile(rf"USER-MEMORY-FAULT: pid {pid} address {int(address, 16)} kind={category}\b").search(log, armed[0].end())
    if not fault:
        raise SystemExit(f"{kind}: missing matching address/PID fault")
    if not re.compile(rf"Process.reclaimProcess: stopped PID {pid}\b").search(log, fault.end()):
        raise SystemExit(f"{kind}: missing process retirement")
print("Protection faults: PASS guard read and read-only write trapped")
