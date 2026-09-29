"""Check the isolated RAM-backed protection fixture's serial evidence."""
import pathlib
import re
import sys

text = pathlib.Path(sys.argv[1]).read_text(errors="replace")
assert "map-check: FAIL" not in text, "fixture reported failure"
read = text.index("map-check: read PASS; attempting forbidden write at 51000000")
loaded = list(re.finditer(r"Loaded module map-check\.app w/ process ID\s+(\d+)\b", text[:read]))
assert loaded, "no identity for the protection fixture"
pid = int(loaded[-1].group(1))
fault = re.search(
    rf"USER-MEMORY-FAULT: pid\s+{pid}\s+address\s+{0x51000000}\s+kind=write-protection\b",
    text[read:])
assert fault, "no protection write fault at the read-only alias"
after = read + fault.end()
assert f"Process.reclaimProcess: stopped PID {pid}" in text[after:], "fixture was not stopped"
assert "desktop: display info ready" in text[after:], "desktop did not start after fixture fault"
print("PASS: writable request denied, readonly read succeeds, forbidden write faults; desktop survives")
