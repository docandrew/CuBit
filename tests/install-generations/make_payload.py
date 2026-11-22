#!/usr/bin/env python3
"""The install payload tests/install-generations installs (SYS-001 installer
v2): the development disk's files, copied into the payload (installer.svc
may read only the medium's install/ directory on @nvme:0: its manifest
scopes; the live medium @cd:0 is readable whole), every program among them
with its SHA-256 (a component: the installer also stores it by content in the
installed storehouse and checks it as it copies), the boot files from the
build, and INIT as the installed root's startup plan (init.ccl).

usage: make_payload.py DEV_DISK OUT_DIR INIT EFI KERNEL INITRD
"""
import pathlib
import stat
import subprocess
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[2] / "tools"))
import install_payload  # noqa: E402

DEBUGFS_SECONDS = 120
# Never installed from the development disk: the payload brings its own
# startup plan, and the install directory is the payload itself.
SKIPPED = {"init.ccl", "install", "lost+found"}


def debugfs(disk, request):
    return subprocess.run(["debugfs", "-R", request, str(disk)], capture_output=True,
                          timeout=DEBUGFS_SECONDS, check=True).stdout


def files_of(disk):
    """(path, mode) of every regular file, and every directory, below the root."""
    files, directories, pending = [], [], [""]
    while pending:
        parent = pending.pop()
        for line in debugfs(disk, f"ls -p /{parent}").decode(errors="replace").splitlines():
            fields = line.split("/")
            # /inode/mode/uid/gid/name/size/
            if len(fields) < 7 or fields[5] in ("", ".", ".."):
                continue
            mode, name = int(fields[2], 8), fields[5]
            path = f"{parent}/{name}" if parent else name
            if path.split("/")[0] in SKIPPED:
                continue
            if stat.S_ISDIR(mode):
                pending.append(path)
                directories.append(path)
            elif stat.S_ISREG(mode):
                files.append((path, mode))
    return sorted(files), sorted(directories)


def main():
    disk, out, init, efi, kernel, initrd = (pathlib.Path(a) for a in sys.argv[1:7])
    files, directories = files_of(disk)
    root = {"init.ccl": init.read_bytes()}
    for path, _ in files:
        root[path] = debugfs(disk, f"cat /{path}")
    # Programs in the payload carry their SHA-256 (install_payload.entries).
    components = sum(1 for path, data in root.items() if install_payload.is_component(data))
    entries = install_payload.entries(efi.read_bytes(), kernel.read_bytes(), initrd.read_bytes(),
                                      root, directories=frozenset(directories))
    for name, data in entries.items():
        target = out / name
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(data)
    print(f"install-generations: payload of {len(root)} root files ({components} programs)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
