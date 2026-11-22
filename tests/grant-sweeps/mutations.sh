#!/usr/bin/env bash
#  Mutation checks for Grant_Sweeps: each mutation of the production source
#  must make the hosted test fail. Run in the Nix shell.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root="$here/../.."
src=$root/kernel/src
work=$(mktemp -d "${TMPDIR:-/home/doc/cubit-build-tmp}/grant-sweeps-mutations.XXXXXX")
trap 'rm -rf "$work"' EXIT
mutate() {
    local name=$1 file=$2 from=$3 to=$4
    rm -rf "$work/src" "$work/build"; mkdir -p "$work/src"
    cp "$src/grant_sweeps.ads" "$src/grant_sweeps.adb" "$here/main.adb" "$work/src/"
    python3 - "$work/src/$file" "$from" "$to" <<'PY' || { echo "MUTATION $name: pattern not found"; return 2; }
import sys
p, a, b = sys.argv[1:]
s = open(p).read()
if a not in s: sys.exit(1)
open(p, 'w').write(s.replace(a, b, 1))
PY
    cat > "$work/m.gpr" <<GPR
project M is
   for Source_Dirs use ("src");
   for Object_Dir use "build/obj"; for Exec_Dir use "build"; for Main use ("main.adb");
   package Compiler is for Default_Switches ("Ada") use ("-O1", "-gnat2022", "-gnata", "-gnato"); end Compiler;
end M;
GPR
    (cd "$root/kernel" && alr exec -- gprbuild -p -q -P "$work/m.gpr") >/dev/null 2>&1 || { echo "MUTATION $name: build failed (caught by compiler)"; return 0; }
    if timeout 60 "$work/build/main" > "$work/out" 2>&1; then
        echo "MUTATION $name: NOT CAUGHT"; return 1
    fi
    echo "MUTATION $name: caught ($(tail -1 "$work/out"))"
}
fail=0
#  The kernel bug's shape: a walk that does not hear of unlinks.
mutate pass-ignored grant_sweeps.adb "                C.Ahead := Following;" "                null;" || fail=1
#  Every list's cursors moved, not only the unlinked list's.
mutate pass-any-list grant_sweeps.adb "if C.Ahead = S and then C.Key = Key then" "if C.Ahead = S then" || fail=1
#  Only the innermost walk hears of unlinks.
mutate pass-innermost-only grant_sweeps.adb "            C := C.Outer;" "            C := null;" || fail=1
#  Advance after the visit (the visit may unlink the grant under the cursor).
mutate advance-late grant_sweeps.adb "            Here.Ahead := Next (Key, S);
            Visit (S);" "            Visit (S);
            Here.Ahead := Next (Key, S);" || fail=1
exit $fail
