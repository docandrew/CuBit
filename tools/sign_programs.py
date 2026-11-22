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
"""
import argparse
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


def run(command):
    return subprocess.run([str(part) for part in command], capture_output=True, text=True, timeout=600)


def sign_directory(directory, signing, tool, debug_dir, exclude):
    splitter = image_debug.Splitter(debug_dir, "sign-programs")
    signed = kept = 0
    for path in sorted(Path(directory).iterdir()):
        if path.name in exclude or path.is_symlink() or not path.is_file():
            continue
        data = path.read_bytes()
        payload = image_debug.signed_payload(data)
        if not image_debug.is_executable(payload if payload is not None else data):
            continue
        if payload is not None:
            verdict = run([tool, "verify", "--anchor", signing / CA_CERTIFICATE, path])
            if verdict.returncode:
                raise SystemExit(f"sign_programs: {path} carries a signature that does not verify under "
                                 f"{signing / CA_CERTIFICATE}: {verdict.stdout.strip() or verdict.stderr.strip()}")
            if image_debug.debug_bytes(payload) == 0:
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
        signed += 1
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
    for directory in args.directories:
        signed, kept = sign_directory(directory, args.signing, args.sign_tool, args.debug_dir,
                                      set(args.exclude))
        print(f"sign_programs: {directory}: {signed} signed, {kept} already signed")


if __name__ == "__main__":
    main()
