#!/usr/bin/env bash
#  Mutation checks for the reply slot (BUG-004): each mutation of
#  replaceReplyCap must make the hosted test fail. Run in the Nix shell.
set -uo pipefail
here=$(cd "$(dirname "$0")" && pwd)
root="$here/../.."
work=$(mktemp -d "${TMPDIR:-/home/doc/cubit-build-tmp}/reply-slot-mutations.XXXXXX")
trap 'rm -rf "$work"' EXIT
mutate() {
    local name=$1 from=$2 to=$3
    rm -rf "$work/src" "$work/build"; mkdir -p "$work/src"
    for f in capabilities.ads capabilities-operations.ads capabilities-operations.adb \
             call_sequences.ads call_sequences.adb; do
        cp "$root/kernel/src/$f" "$work/src/"
    done
    cp "$here/main.adb" "$work/src/"
    python3 - "$work/src/capabilities-operations.adb" "$from" "$to" <<'PY' || { echo "MUTATION $name: pattern not found"; return 2; }
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
    echo "MUTATION $name: caught ($(grep -m1 FAIL "$work/out"))"
}
fail=0
# The bug itself: the old authority is overwritten and dropped.
mutate drop-displaced "            displaced := slot;
        else" "            displaced := NULL_CAPABILITY;
        else" || fail=1
# The new authority is not installed.
mutate keep-old "        slot := incoming;
    end replaceReplyCap;" "        null;
    end replaceReplyCap;" || fail=1
exit $fail
