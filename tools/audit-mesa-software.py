#!/usr/bin/env python3
"""Inventory a hosted Mesa binary's dependencies; NOT a native-portability proof."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess


def audit(binary):
    dynamic = subprocess.check_output(["readelf", "-d", str(binary)], text=True)
    symbols = subprocess.check_output(
        ["nm", "-D", "--undefined-only", str(binary)], text=True
    )
    imports = sorted({line.split()[-1].split("@")[0]
                      for line in symbols.splitlines() if line.strip()})
    groups = {
        "llvm": [s for s in imports if s.startswith("LLVM")],
        "drm": [s for s in imports if s.startswith("drm")],
        "threads": [s for s in imports if s.startswith("pthread_")],
        "mapping": [s for s in imports if s in
                    {"mmap", "mmap64", "munmap", "mprotect", "madvise"}],
        "clock": [s for s in imports if s.startswith("clock_")],
    }
    return {
        "scope": "Linux-hosted direct ELF dependencies only; transitive dependencies not audited",
        "binary": str(binary.resolve()),
        "sha256": hashlib.file_digest(binary.open("rb"), "sha256").hexdigest(),
        "needed": re.findall(r"\(NEEDED\).*?\[(.*?)\]", dynamic),
        "groups": groups,
        "undefined_symbols": imports,
        "native_ready": False,
        "limitations": [
            "Absent direct mprotect import does not clear LLVM's JIT mapping requirements",
            "Import presence does not establish runtime path reachability",
            "No capability, ABI, memory ordering, or presentation validation",
        ],
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("binary", type=Path)
    args = parser.parse_args()
    print(json.dumps(audit(args.binary), indent=2))
