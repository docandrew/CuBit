#!/usr/bin/env bash
#  Mutation checks for the grant doorbell units: each mutation of the
#  production source must make the hosted test fail. Run in the Nix shell.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root="$here/../.."
work=$(mktemp -d "${TMPDIR:-/home/doc/cubit-build-tmp}/grant-doorbell-mutations.XXXXXX")
trap 'rm -rf "$work"' EXIT
mutate() {
    local name=$1 file=$2 from=$3 to=$4
    rm -rf "$work/src" "$work/build"; mkdir -p "$work/src"
    cp "$root/kernel/src/grant_doorbells.ad"? "$root/kernel/src/grant_bell_lists.ad"? "$here/main.adb" "$work/src/"
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
    if "$work/build/main" > "$work/out" 2>&1; then
        echo "MUTATION $name: NOT CAUGHT"; return 1
    fi
    echo "MUTATION $name: caught ($(tail -1 "$work/out"))"
}
fail=0
mutate newly-always grant_doorbells.adb "Newly := not B.Pending;" "Newly := True;" || fail=1
mutate take-keeps grant_doorbells.adb "        Taken := B.Pending;
        B.Pending := False;" "        Taken := B.Pending;" || fail=1
mutate finish-keeps grant_doorbells.adb "        Was_Pending := B.Pending;
        B.Pending := False;" "        Was_Pending := B.Pending;" || fail=1
mutate ring-after-end grant_doorbells.adb "        if B.Ended then
            Outcome := Ended_Grant;" "        if False then
            Outcome := Ended_Grant;" || fail=1
mutate list-lifo grant_bell_lists.adb "        S := L.Head;" "        S := L.Tail;" || fail=1
mutate remove-no-tail grant_bell_lists.adb "            L.Tail := P;" "            null;" || fail=1
exit $fail
