#!/usr/bin/env python3
"""Convert a PEM CA bundle into tls.svc's trust store: concatenated DER.

Usage: pem_bundle_to_der.py BUNDLE.pem OUT.der

SPARKTLS limits: at most 200 trust anchors (Max_Root_Pool_Size) of at most
8192 DER bytes each (Max_Cert_DER). Larger certificates are skipped with a
warning; more than 200 is an error rather than a silent truncation.
"""
import base64
import sys

MAX_ROOTS = 200
MAX_DER = 8192


def main():
    source, target = sys.argv[1], sys.argv[2]
    roots, skipped = [], 0
    block = None
    for line in open(source, encoding="utf-8", errors="strict"):
        line = line.strip()
        if line == "-----BEGIN CERTIFICATE-----":
            block = []
        elif line == "-----END CERTIFICATE-----":
            der = base64.b64decode("".join(block), validate=True)
            if der[:1] != b"\x30":
                raise SystemExit(f"{source}: certificate is not a DER SEQUENCE")
            if len(der) > MAX_DER:
                skipped += 1
            else:
                roots.append(der)
            block = None
        elif block is not None:
            block.append(line)
    if not roots:
        raise SystemExit(f"{source}: no certificates")
    if len(roots) > MAX_ROOTS:
        raise SystemExit(f"{source}: {len(roots)} roots exceed SPARKTLS's {MAX_ROOTS}")
    with open(target, "wb") as out:
        for der in roots:
            out.write(der)
    note = f", {skipped} over {MAX_DER} bytes skipped" if skipped else ""
    print(f"pem_bundle_to_der: {len(roots)} trust anchors{note}")


if __name__ == "__main__":
    main()
