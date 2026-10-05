"""Reject MMX in the native Ada archive: it aliases the caller's x87 stack."""
import re
import subprocess
import sys
from pathlib import Path

archive = Path(sys.argv[1])
text = subprocess.check_output(["objdump", "-d", str(archive)], text=True)
violations = [line for line in text.splitlines() if re.search(r"%mm[0-7]\b", line)]
if violations:
    raise SystemExit("PENNY-NATIVE-MMX: FAIL (clean-rebuild the native library)\n" + "\n".join(violations[:20]))
print("PENNY-NATIVE-MMX: PASS no MMX register instructions in native archive")
