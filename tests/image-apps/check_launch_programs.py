#!/usr/bin/env python3
"""Every program the Apps menu can launch ships in every laptop image, and
is in the Desktop's launch table.

The Apps menu comes from desktop.launch.* settings (Launch_Entry ...
action => (Launch_Action.Program "name.app")) in the system descriptions.
An entry whose program is not on the image's optical tree opens nothing on
the laptop. That is how the Text editor went missing from the USB image (2026-11-16).
Desktop starts a program with OP_LAUNCH, which procmgr refuses unless the
Desktop's manifest names it (may_launch, CAP-012); an entry missing there
opens nothing either.

usage: check_launch_programs.py [REPO_ROOT]
"""
import pathlib
import re
import sys

SYSTEMS = ("system.ccl", "tests/hardware/system-live.ccl")
# Every system description whose Apps menu the Desktop serves.
MENUS = SYSTEMS + ("tests/hardware/system-installed.ccl", "tests/llm/system-llm-test.ccl")
DESKTOP_MANIFEST = "userspace/services/desktop/manifest.ccl"
LAUNCH = re.compile(r'\(Launch\s+"([^"]+)"\)')
IMAGES = ("images/laptop-usb.ccl", "images/preview.ccl", "images/laptop-build.ccl",
          "images/laptop-build-test.ccl", "images/laptop-install.ccl")
PROGRAM = re.compile(r'Launch_Action\.Program\s+"([^"]+)"')
SHIPPED = re.compile(r'\(file\s+optical\s+"[^"]+"\s+"apps/([^"]+)"\)')


def main() -> int:
    root = pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else ".")
    programs = {}
    for system in SYSTEMS:
        for name in PROGRAM.findall((root / system).read_text()):
            programs.setdefault(name, system)
    if not programs:
        print("check_launch_programs: FAIL no Launch_Action.Program entries found")
        return 1
    failures = 0
    for image in IMAGES:
        shipped = set(SHIPPED.findall((root / image).read_text()))
        for name, system in sorted(programs.items()):
            if name not in shipped:
                print(f"check_launch_programs: FAIL {image} lacks apps/{name} "
                      f"(an Apps menu entry in {system})")
                failures += 1
    table = set(LAUNCH.findall((root / DESKTOP_MANIFEST).read_text()))
    menu = {}
    for system in MENUS:
        for name in PROGRAM.findall((root / system).read_text()):
            menu.setdefault(name, system)
    for name, system in sorted(menu.items()):
        if name not in table:
            print(f"check_launch_programs: FAIL {DESKTOP_MANIFEST} may_launch lacks {name} "
                  f"(an Apps menu entry in {system})")
            failures += 1
    if failures:
        return 1
    print(f"check_launch_programs: PASS {len(programs)} launchable programs in "
          f"{len(IMAGES)} laptop images; {len(menu)} in Desktop's launch table")
    return 0


if __name__ == "__main__":
    sys.exit(main())
