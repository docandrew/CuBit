#!/usr/bin/env bash
# Fails if tls.svc's trust store (kernel/build/tls-roots.der, made from
# CUBIT_CA_BUNDLE) holds a root listed in distrusted.txt.
# Run inside nix develop after `make -C kernel build/tls-roots.der`.
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
store=${1:-$here/../../kernel/build/tls-roots.der}
python3 - "$store" "$here/distrusted.txt" <<'PY'
import hashlib, sys

store, listing = sys.argv[1], sys.argv[2]
data = open(store, "rb").read()
distrusted = {}
for line in open(listing, encoding="utf-8"):
    if line.strip() and not line.startswith("#"):
        digest, name = line.split(maxsplit=1)
        distrusted[digest] = name.strip()

def der_length(offset):
    """Total length of the DER SEQUENCE starting at offset."""
    if data[offset] != 0x30:
        raise SystemExit(f"TLS-ROOTS: no DER SEQUENCE at byte {offset}")
    first = data[offset + 1]
    if first < 0x80:
        return 2 + first
    count = first & 0x7F
    return 2 + count + int.from_bytes(data[offset + 2 : offset + 2 + count], "big")

offset, roots, found = 0, 0, []
while offset < len(data):
    size = der_length(offset)
    digest = hashlib.sha256(data[offset : offset + size]).hexdigest()
    if digest in distrusted:
        found.append(distrusted[digest])
    offset += size
    roots += 1
if found:
    raise SystemExit("TLS-ROOTS: FAIL, distrusted roots present: " + ", ".join(found))
print(f"TLS-ROOTS: {roots} roots, none of {len(distrusted)} distrusted")
print("TLS-ROOTS: PASS")
PY
