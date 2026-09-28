"""Check the isolated RAM-backed protection fixture's serial evidence."""
import pathlib
import re
import sys

text = pathlib.Path(sys.argv[1]).read_text(errors="replace")
assert "map-check: FAIL" not in text, "fixture reported failure"
read = text.index("map-check: read PASS; attempting forbidden write at 51000000")
fault = re.search(r"User page-protection write violation:\s*(?:0x)?0*51000000\b", text[read:], re.I)
assert fault, "no protection write fault at the read-only alias"
after = read + fault.end()
assert "desktop: display info ready" in text[after:], "desktop did not start after fixture fault"
print("PASS: writable request denied, readonly read succeeds, forbidden write faults; desktop survives")
