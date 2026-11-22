#!/usr/bin/env python3
"""Regression for the 2026-11-06 laptop install hang (installer.svc in
Waiting_Reply forever while filesystem.svc and ufs.drv sat idle): no call on
the install path, or on procmgr's boot path to the startup profile and the
current generation, may wait forever (IPC-000 rule 6). Each is named here
with the deadline that bounds it; a Wait_Forever in any of these units, or a
deadline constant missing, fails. (Before the fix every one of these calls
was Wait_Forever.)

usage: check_deadlines.py   (from anywhere)
"""
import pathlib
import re
import sys

# usage: check_deadlines.py [TREE] (default: this checkout).
ROOT = pathlib.Path(sys.argv[1]) if len(sys.argv) > 1 else pathlib.Path(__file__).resolve().parents[2]
# Units whose every call has an explicit deadline.
NO_WAIT_FOREVER = [
    "userspace/services/installer/main.adb",
    "userspace/lib/disk-install/system_install.adb",
    "userspace/lib/disk-install/install_sources.adb",
    "userspace/runtime/gnat/cubit-block_client.adb",
    "userspace/runtime/gnat/cubit-disk_install-client.adb",
    "userspace/ccl/native/ccl_installer.adb",
]
# The named deadlines (unit, constant).
DEADLINES = [
    ("userspace/lib/disk-install/system_install.ads", "Device_Request_Deadline_Ms"),
    ("userspace/lib/disk-install/system_install.ads", "Service_Deadline_Ms"),
    ("userspace/lib/disk-install/install_sources.ads", "Request_Deadline_Ms"),
    ("userspace/ccl/native/ccl_installer.adb", "Answer_Deadline_Ms"),
    ("userspace/services/procmgr/main.adb", "Startup_Profile_Deadline_Ms"),
    ("userspace/services/procmgr/main.adb", "Boot_Read_Deadline_Ms"),
    ("userspace/services/procmgr/main.adb", "File_Request_Deadline_Ms"),
]


def source(unit):
    path = ROOT / unit
    return path.read_text() if path.exists() else ""


def code(text):
    return "\n".join(line.split("--", 1)[0] for line in text.splitlines())


def main():
    failures = 0
    for unit in NO_WAIT_FOREVER:
        if not (ROOT / unit).exists():
            print(f"FAIL: {unit} is missing")
            failures += 1
            continue
        text = code(source(unit))
        # A server idle between requests may wait for activity without end;
        # a call may not.
        found = [n + 1 for n, line in enumerate(text.splitlines())
                 if "Wait_Forever" in line and "Wait_For_Activity_Until" not in line]
        ok = not found
        print(("PASS: " if ok else "FAIL: ") + f"{unit}: no call waits forever" +
              ("" if ok else f" (lines {found})"))
        failures += 0 if ok else 1
    # procmgr's file reads take the caller's deadline.
    procmgr = code(source("userspace/services/procmgr/main.adb"))
    body = re.search(r"function readFileFromFS \(name : String; Within_Ms : Unsigned_64\).*?end readFileFromFS;",
                     procmgr, re.S)
    ok = body is not None and "Wait_Forever" not in body.group(0)
    print(("PASS: " if ok else "FAIL: ") + "procmgr's readFileFromFS takes an explicit deadline")
    failures += 0 if ok else 1
    # A deadline is a number, or another named deadline of the same unit.
    for unit, name in DEADLINES:
        named = "|".join(re.escape(other) for u, other in DEADLINES if u == unit and other != name)
        ok = re.search(rf"\b{name}\s*:\s*constant\s*:=\s*([0-9_]+|(?:{named})\b)",
                       code(source(unit))) is not None
        print(("PASS: " if ok else "FAIL: ") + f"{unit}: named deadline {name}")
        failures += 0 if ok else 1
    print("INSTALL-DEADLINES: " + ("PASS" if failures == 0 else f"FAIL ({failures})"))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
