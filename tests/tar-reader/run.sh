#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
#  SELF-003 step 1: hosted tests of the tar reader (userspace/lib/archive),
#  then archives written by Python's tarfile (ustar, GNU, pax) and git
#  archive, walked and compared. --prove adds gnatprove level 2 (no
#  unproved check, nothing justified). Run in the Nix shell, proofs under a
#  memory cap:
#    systemd-run --user --scope -p MemoryMax=24G -p MemorySwapMax=0 \
#      bash tests/tar-reader/run.sh --prove
set -euo pipefail
here=$(cd "$(dirname "$0")" && pwd)
cd "$here/../../kernel"
alr exec -- gprbuild -p -q -P "$here/tar_reader_tests.gpr"
"$here/build/main"
fixtures="$here/build/fixtures"
rm -rf "$fixtures"
python3 -I "$here/fixtures.py" "$fixtures"
for name in ustar gnu pax git link cut; do
    "$here/build/walk" "$fixtures/$name.tar" > "$fixtures/$name.walk"
    if ! cmp -s "$fixtures/$name.walk" "$fixtures/$name.expected"; then
        echo "tar-reader: $name.tar walked differently:" >&2
        diff "$fixtures/$name.expected" "$fixtures/$name.walk" >&2 || true
        exit 1
    fi
done
"$here/build/walk" "$fixtures/link.tar" skip > "$fixtures/link.skip-walk"
cmp -s "$fixtures/link.skip-walk" "$fixtures/link.skip-expected" ||
    { echo "tar-reader: link.tar (skip) walked differently" >&2; exit 1; }
echo "tar-reader: interop PASS (ustar, gnu, pax, git archive, symlink, truncated)"
# Unpacking (Tar_Unpack over the Linux host's directories): the trees
# Python's tarfile extracts, and nothing left after a refusal or a failed
# write.
out="$fixtures/out"
mkdir -p "$out"
expect_unpack() {   # archive directory expected-first-word
    local got
    got=$("$here/build/unpack" "$fixtures/$1.tar" "$out" "$2" ${4:-})
    [[ $got == "$3"* ]] || { echo "tar-reader: unpack $1: $got" >&2; exit 1; }
}
for name in ustar gnu pax git; do
    expect_unpack "$name" "$name-tree" UNPACKED
    python3 -I - "$fixtures/$name.tar" "$out/$name-tree" <<'PY' || exit 1
import os, sys, tarfile
archive, tree = sys.argv[1:]
with tarfile.open(archive) as tar:
    for m in tar.getmembers():
        p = os.path.join(tree, m.name.rstrip('/'))
        if m.isdir():
            assert os.path.isdir(p), p
        elif m.isfile():
            assert open(p, 'rb').read() == tar.extractfile(m).read(), p
names = sorted(os.path.relpath(os.path.join(d, f), tree) for d, ds, fs in os.walk(tree) for f in ds + fs)
with tarfile.open(archive) as tar:
    want = set()
    for m in tar.getmembers():
        parts = m.name.rstrip('/').split('/')
        for i in range(1, len(parts) + 1):
            want.add('/'.join(parts[:i]))
assert names == sorted(want), (names, sorted(want))
PY
done
expect_unpack pax pax-tree TARGET_EXISTS
expect_unpack traverse t-tree "REFUSED UNSAFE_PATH ../evil.txt"
expect_unpack dup d-tree WRITE_FAILED
expect_unpack link l-tree "REFUSED SPECIAL_ENTRY"
expect_unpack link l-tree "UNPACKED 1 3 1" skip
expect_unpack cut c-tree "REFUSED TRUNCATED"
expect_unpack pax "../escape" BAD_DIRECTORY
expect_unpack pax "a/b" BAD_DIRECTORY
left=$(cd "$out" && ls -A | sort | tr '\n' ' ')
[[ $left == "git-tree gnu-tree l-tree pax-tree ustar-tree " ]] ||
    { echo "tar-reader: unpack left: $left" >&2; exit 1; }
echo "tar-reader: unpack PASS (trees match tarfile; refusals and failed writes leave nothing)"
if [[ ${1:-} == --prove ]]; then
    alr exec -- gnatprove -P "$here/tar_reader_tests.gpr" -u tar_archives.adb --level=2 -j2 \
        --checks-as-errors=on --report=fail 2>&1 | tee "$here/build/proof.log"
    if grep -qE "(medium|high|error|warning):" "$here/build/proof.log" ||
       grep -qs "pragma Annotate\|Justif" "$here/../../userspace/lib/archive/tar_archives.ads" \
         "$here/../../userspace/lib/archive/tar_archives.adb"; then
        echo "tar-reader: proof FAILED" >&2
        exit 1
    fi
    echo "tar-reader: proof OK"
fi
