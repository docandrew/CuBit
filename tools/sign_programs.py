#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""The sign step for programs make places in a staging directory
(SIGN-001, docs/signed-binaries.md section 7).

For every ELF executable directly in each DIRECTORY (the kernel, the boot
trusted base, is left out by --exclude):

1. a file already signed must verify under the signing directory's CA
   (ccl-sign verify); one signed by anything else, or damaged, stops the
   build. Signed and without debug information: nothing to do.
2. otherwise its debug information goes to the host debug directory by
   GNU build-id (tools/image_debug.py, as image staging always did), and
   the stripped program is signed in place with ccl-sign: signing is the
   last step, after strip, because strip would drop the envelope. A
   ccl-build output (signed with its debug information, as the storehouse
   keeps it) is stripped and signed again the same way.

Signing is deterministic (RFC 6979), so a second run changes nothing. A
build without a key stops with a message naming make signing-key.

A second run also verifies nothing again (BLD-001): every file this step
signed or verified is recorded in DEBUG_DIR/images/sign-programs.verified.json
with its size, inode, modification and change times and SHA-256, under the
SHA-256 of the CA certificate and of the sign tool. A file whose stat still
matches its record under the same CA and tool holds the bytes that were
verified, so it is counted as already signed without reading it; anything
else (a new or rewritten file, another CA, a rebuilt ccl-sign) goes through
the full check above. The change time cannot be set back from user space,
so a rewrite that restores the modification time is still seen.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
import image_debug  # noqa: E402

CA_CERTIFICATE = "builder-ca.der"
LEAF_KEY = "builder.key"
VERIFIED_FORMAT = "cubit-sign-programs-verified-1"


def run(command):
    return subprocess.run([str(part) for part in command], capture_output=True, text=True, timeout=600)


def file_digest(path):
    with open(path, "rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def stat_key(path):
    """What must not have changed since a file was verified."""
    info = path.stat()
    return [info.st_size, info.st_ino, info.st_mtime_ns, info.st_ctime_ns]


class Verified:
    """The record of files already signed or verified (see the module text)."""

    def __init__(self, debug_dir, signing, tool):
        self.path = Path(debug_dir) / "images" / "sign-programs.verified.json"
        self.key = {"anchor_sha256": file_digest(signing / CA_CERTIFICATE),
                    "tool_sha256": file_digest(tool)}
        self.before = {}
        try:
            document = json.loads(self.path.read_text())
            if document.get("format") == VERIFIED_FORMAT and document.get("key") == self.key:
                self.before = document.get("directories", {})
        except (OSError, ValueError):
            pass
        self.after = {}

    def unchanged(self, directory, path):
        """None for a file to check; else whether the unchanged file is a program."""
        entry = self.before.get(str(directory), {}).get(path.name)
        if entry is None or entry.get("stat") != stat_key(path):
            return None
        self.note(directory, path, entry["sha256"], entry["program"])
        return entry["program"]

    def note(self, directory, path, sha256=None, program=True):
        self.after.setdefault(str(directory), {})[path.name] = {
            "stat": stat_key(path), "sha256": sha256 or file_digest(path), "program": program}

    def save(self):
        if self.after == self.before:
            return
        document = {"format": VERIFIED_FORMAT, "key": self.key, "directories": self.after}
        self.path.parent.mkdir(parents=True, exist_ok=True)
        pending = self.path.with_name(self.path.name + f".{os.getpid()}.tmp")
        pending.write_text(json.dumps(document, indent=1, sort_keys=True) + "\n")
        os.replace(pending, self.path)


def sign_directory(directory, signing, tool, debug_dir, exclude, verified):
    splitter = image_debug.Splitter(debug_dir, "sign-programs")
    signed = kept = 0
    for path in sorted(Path(directory).iterdir()):
        if path.name in exclude or path.is_symlink() or not path.is_file():
            continue
        program = verified.unchanged(directory, path)
        if program is not None:
            kept += program
            continue
        data = path.read_bytes()
        payload = image_debug.signed_payload(data)
        if not image_debug.is_executable(payload if payload is not None else data):
            verified.note(directory, path, hashlib.sha256(data).hexdigest(), program=False)
            continue
        if payload is not None:
            verdict = run([tool, "verify", "--anchor", signing / CA_CERTIFICATE, path])
            if verdict.returncode:
                raise SystemExit(f"sign_programs: {path} carries a signature that does not verify under "
                                 f"{signing / CA_CERTIFICATE}: {verdict.stdout.strip() or verdict.stderr.strip()}")
            if image_debug.debug_bytes(payload) == 0:
                verified.note(directory, path, hashlib.sha256(data).hexdigest())
                kept += 1
                continue
            data = payload
        stripped = splitter.split(path.name, data)
        pending = path.with_name(f".{path.name}.unsigned.{os.getpid()}")
        pending.write_bytes(stripped)
        shutil.copymode(path, pending)
        result = run([tool, "sign", "--signing", signing, pending, path])
        pending.unlink()
        if result.returncode:
            raise SystemExit(f"sign_programs: {path}: {result.stderr.strip() or result.stdout.strip()}")
        verified.note(directory, path)
        signed += 1
    if splitter.records or not (splitter.debug_dir / "images" / "sign-programs.build-ids.json").is_file():
        record_splits(splitter)
    return signed, kept


def record_splits(splitter):
    """Keep what was stripped before signing (debug/images/sign-programs.build-ids.json):
    the image audit maps a shipped payload back to the program as built. A later
    run that finds programs already signed records nothing new, so earlier
    records stay unless the same program was stripped again."""
    path = splitter.debug_dir / "images" / f"{splitter.image_name}.build-ids.json"
    earlier = []
    if path.is_file():
        earlier = json.loads(path.read_text()).get("binaries", [])
    fresh = {r["destination"] for r in splitter.records}
    splitter.records = [r for r in earlier if r["destination"] not in fresh] + splitter.records
    splitter.write_manifest()


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("directories", nargs="+", type=Path)
    parser.add_argument("--signing", type=Path, required=True, help="the signing directory (CUBIT_SIGNING)")
    parser.add_argument("--sign-tool", type=Path, required=True, help="ccl-sign")
    parser.add_argument("--debug-dir", type=Path, required=True,
                        help="host directory for the separated debug files (by build-id)")
    parser.add_argument("--exclude", action="append", default=[], help="a file name to leave unsigned")
    args = parser.parse_args()
    if not (args.signing / LEAF_KEY).is_file():
        raise SystemExit(f"sign_programs: no signing key in {args.signing}; make one with "
                         "make -C kernel signing-key (or point CUBIT_SIGNING at yours)")
    verified = Verified(args.debug_dir, args.signing, args.sign_tool)
    for directory in args.directories:
        signed, kept = sign_directory(directory, args.signing, args.sign_tool, args.debug_dir,
                                      set(args.exclude), verified)
        print(f"sign_programs: {directory}: {signed} signed, {kept} already signed")
    verified.save()


if __name__ == "__main__":
    main()
