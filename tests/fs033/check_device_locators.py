#!/usr/bin/env python3
"""FS-033: places are named by alias (@system, @boot), never by device.

Fails when a CCL or Ada string literal outside the reviewed allowlist
names a device volume ("@nvme:0/...", "@ufs:0", "@ata:0", "@usb:0",
"@virtio...:N", "@mem:0", "@cd:0"). Comments are not checked: a comment
may say which device an alias resolves to. The allowlist holds the
places where the device itself is the subject: filesystem.svc, devmgr,
the storage drivers, the installer's target disk, the startup plans'
volume bindings, and tests of a specific device.

Usage: python3 -I tests/fs033/check_device_locators.py [repo-root]
"""
import os
import re
import sys

DEVICE = re.compile(r'@(nvme|ufs|ata|usb|virtio[a-z-]*|mem|cd|sd|mmc)[a-z]*:[0-9]+')

# Paths (prefixes, relative to the repo root) where naming a device is the
# point. Every entry says why.
ALLOW = {
    "userspace/services/filesystem/": "filesystem.svc resolves aliases to volumes",
    "userspace/services/devmgr/": "devmgr attaches volumes",
    "userspace/services/nvme/": "storage driver",
    "userspace/services/ufs/": "storage driver",
    "userspace/services/ata/": "storage driver",
    "userspace/services/xhci/": "storage driver (USB mass storage)",
    "userspace/services/installer/": "the installer chooses a target disk",
    "userspace/apps/storage-check/": "tests named devices",
    "userspace/apps/bench-storage/": "benchmarks a named device",
    "userspace/apps/filesystem-scope-check/": "tests scope matching across spellings",
    "userspace/apps/files/files_places": "captions a device's root",
    "userspace/runtime/tests/": "runtime path tests",
    "userspace/runtime/gnat/cubit-path_names": "path name parsing",
    "userspace/runtime/gnat/cubit-filesystem_places": "lists present volumes",
    "userspace/runtime/gnat/cubit-volume_descriptions": "volume names",
    "tests/fs033/": "this check's own fixtures",
    "tests/headless/": "headless plans bind volumes for a test machine",
    "tests/hardware/": "hardware plans bind volumes for a machine",
    "tests/install-generations/": "install tests name the target disk",
    "images/": "image plans bind volumes",
    "tests/device-builds/": "device build plans",
    "tests/usb-disk/native/": "the test writes the USB sticks it attaches",
    "tests/usb-eject/": "the test plugs, writes and ejects named USB sticks",
}


# Files still naming devices, to move in FS-033 slice 2 (the list only
# shrinks; a new file here needs review). Path-parsing tests that use
# device names as data move to ALLOW instead.
PENDING = {
    "tests/acpi-native-entry/fixtures/actual/cubit-path_names.ads",
    "tests/agent/agent_main.adb",
    "tests/binutils/check/main.adb",
    "tests/binutils/check/manifest.ccl",
    "tests/binutils/compare.ccl",
    "tests/ccl-build-native/check/manifest.ccl",
    "tests/ccl-file-dialog/main.adb",
    "tests/ccl-install/main.adb",
    "tests/ccl-programs/main.adb",
    "tests/ccl-remote/remote_wire_tests.adb",
    "tests/compositor/trace-native/archive/main.adb",
    "tests/compositor/trace-native/archive/manifest.ccl",
    "tests/config-turso/native/manifest.ccl",
    "tests/files-app/files_places_tests.adb",
    "tests/files-app/files_view_tests.adb",
    "tests/filesystem-journal/native/manifest.ccl",
    "tests/fs-bench/manifest.ccl",
    "tests/fs-coherence/manifest.ccl",
    "tests/gcc/check/main.adb",
    "tests/gcc/check/manifest.ccl",
    "tests/gnat/check/manifest.ccl",
    "tests/large-manifest-probe/manifest.ccl",
    "tests/launch-arguments/main.adb",
    "tests/launch-grants/main.adb",
    "tests/libc-ada/main.adb",
    "tests/metricsview/view/metricsview_view_tests.adb",
    "tests/path-names/main.adb",
    "tests/program-descriptions/main.adb",
    "tests/sched-latency/manifest.ccl",
    "tests/volume-list/main.adb",
    "tests/volume-names/main.adb",
    "userspace/apps/bible/manifest.ccl",
    "userspace/apps/files/manifest.ccl",
    "userspace/apps/install-boot-check/main.adb",
    "userspace/apps/install-boot-check/manifest.ccl",
    "userspace/apps/metricsview/dash_model.ads",
    "userspace/apps/metricsview/manifest.ccl",
    "userspace/apps/shell/main.adb",
    "userspace/apps/text/main.adb",
    "userspace/apps/text/manifest.ccl",
    "userspace/ccl/apps/agent/manifest.ccl",
    "userspace/ccl/apps/ccl-console/manifest.ccl",
    "userspace/ccl/apps/ccl-remote/manifest.ccl",
    "userspace/ccl/apps/ccl-remote/remote_development.adb",
    "userspace/ccl/apps/ccl-workbench/manifest.ccl",
    "userspace/ccl/native/ccl_bible_source.adb",
    "userspace/ccl/native/ccl_places.adb",
    "userspace/ccl/native/ccl_workspace.adb",
    "userspace/ccl/src/ccl_agent_bindings.adb",
    "userspace/ccl/src/ccl_file_bindings.adb",
    "userspace/ccl/src/ccl_workspace.ads",
    "userspace/lib/observatory/observatory_archive_reader.adb",
    "userspace/libc/tests/manifest.ccl",
    "userspace/ports/binutils/manifest.ccl",
    "userspace/ports/doom/cubit-doom_platform.adb",
    "userspace/ports/doom/manifest.ccl",
    "userspace/ports/sameboy/cubit-sameboy_frontend.adb",
    "userspace/ports/sameboy/manifest.ccl",
    "userspace/services/desktop/manifest.ccl",
    "userspace/services/lexicon/manifest.ccl",
    "userspace/services/logstore/manifest.ccl",
    "userspace/services/tls/main.adb",
    "userspace/services/tls/manifest.ccl",
}

SKIP_DIRS = {".git", "build", "obj", "target", "node_modules", "isodir"}


def literals_ccl(line):
    code = line.split("#", 1)[0] if '"' not in line.split("#", 1)[0] else _strip_ccl_comment(line)
    return re.findall(r'"((?:[^"\\]|\\.)*)"', code)


def _strip_ccl_comment(line):
    out, in_str = [], False
    for ch in line:
        if ch == '"':
            in_str = not in_str
        if ch == "#" and not in_str:
            break
        out.append(ch)
    return "".join(out)


def literals_ada(line):
    out, in_str, i = [], False, 0
    code = []
    while i < len(line):
        ch = line[i]
        if not in_str and line.startswith("--", i):
            break
        if ch == '"':
            if in_str and i + 1 < len(line) and line[i + 1] == '"':
                code.append('""')
                i += 2
                continue
            in_str = not in_str
        code.append(ch)
        i += 1
    return re.findall(r'"((?:[^"]|"")*)"', "".join(code))


def main():
    root = sys.argv[1] if len(sys.argv) > 1 else "."
    bad = []
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in SKIP_DIRS]
        for name in filenames:
            if not name.endswith((".ccl", ".adb", ".ads")):
                continue
            path = os.path.join(dirpath, name)
            rel = os.path.relpath(path, root)
            if any(rel.startswith(a) for a in ALLOW) or rel in PENDING:
                continue
            extract = literals_ccl if name.endswith(".ccl") else literals_ada
            with open(path, encoding="utf-8", errors="replace") as f:
                for n, line in enumerate(f, 1):
                    for lit in extract(line):
                        m = DEVICE.search(lit)
                        if m:
                            bad.append(f"{rel}:{n}: {m.group(0)} in \"{lit[:80]}\"")
    for b in bad:
        print("FS-033:", b)
    if bad:
        print(f"FS-033: FAIL {len(bad)} device locators outside the allowlist; "
              "name the place by alias (@system/..., @boot/...)")
        return 1
    print("FS-033: PASS no device locators outside the allowlist")
    return 0


if __name__ == "__main__":
    sys.exit(main())
