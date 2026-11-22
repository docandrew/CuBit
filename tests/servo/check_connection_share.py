#!/usr/bin/env python3
"""Penny's HTTP client share (the Servo fork's components/net/connection_limit.rs)
stays inside the connections its manifest declares (manifest.ccl): the two
numbers are written in two languages and must change together."""
import os
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "userspace/servo/manifest.ccl"
# Penny's Servo fork checkout (userspace/servo/FORKS).
SERVO = pathlib.Path(os.environ.get("SERVO_DIR", ROOT / "userspace/rust/build/servo-work/servo"))
LIMIT = SERVO / "components/net/connection_limit.rs"


def constant(text, name):
    match = re.search(rf"pub const {name}: usize = (\d+);", text)
    assert match, f"{LIMIT.name}: no {name}"
    return int(match.group(1))


def main():
    declared = re.findall(r"\(connections (\d+)\)", MANIFEST.read_text())
    assert len(declared) == 1, f"{MANIFEST.name}: expected one (connections N), found {declared}"
    declared = int(declared[0])
    source = LIMIT.read_text()
    assert constant(source, "DECLARED_CONNECTIONS") == declared, \
        f"DECLARED_CONNECTIONS {constant(source, 'DECLARED_CONNECTIONS')} != manifest {declared}"
    reserved = constant(source, "RESERVED_CONNECTIONS")
    per_host = constant(source, "CONNECTIONS_PER_HOST")
    assert 0 < reserved < declared, (reserved, declared)
    assert 0 < per_host <= declared - reserved, (per_host, declared, reserved)
    print(f"PASS: manifest declares {declared}; HTTP client share {declared - reserved}, "
          f"{per_host} per host")


if __name__ == "__main__":
    try:
        main()
    except AssertionError as error:
        print(f"FAIL: {error}")
        sys.exit(1)
