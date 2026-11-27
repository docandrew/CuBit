#!/usr/bin/env bash
# Mutation check for the deadline heap proof: each mutant must FAIL to prove.
# Run from the Nix shell: tests/wait-deadlines/mutations.sh [kernel-src-dir]
set -u
here=$(cd "$(dirname "$0")" && pwd)
ksrc=${1:-$here/../../kernel/src}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$here/../../kernel"   # alr needs the kernel's workspace

mutate() {  # name file sed-expression
    local name=$1 file=$2 expr=$3 dir="$work/$1"
    mkdir -p "$dir/src"
    cp "$ksrc"/wait_deadlines.ad? "$here"/main.adb "$dir/src/"
    sed -i "$expr" "$dir/src/$file"
    if cmp -s "$dir/src/$file" "$ksrc/$file" 2>/dev/null; then
        echo "MUTANT $name: NOT APPLIED"; return 1
    fi
    sed "s|(\".\", Kernel_Src)|(\"$dir/src\")|; s|for Object_Dir use \"build\"|for Object_Dir use \"$dir/build\"|; s|for Exec_Dir use \"build\"|for Exec_Dir use \"$dir/build\"|" \
        "$here/wait_deadlines_tests.gpr" > "$dir/t.gpr"
    if alr exec -- gnatprove -P"$dir/t.gpr" -u wait_deadlines.adb \
         --level=2 --timeout=30 -j2 --report=fail >"$dir/log" 2>&1 \
       && ! grep -q "medium\|high" "$dir/log"; then
        echo "MUTANT $name: SURVIVED (proof still passes)"; return 1
    fi
    if ! grep -q "medium\|high" "$dir/log"; then
        echo "MUTANT $name: ERROR (no proof failure reported)"; tail -5 "$dir/log"; return 1
    fi
    echo "MUTANT $name: killed -- $(grep -m1 'medium\|high' "$dir/log")"
}

fail=0
# Sift up stops one step early (a smaller key below a larger one).
mutate up-strict wait_deadlines.adb 's/while J > 1 and then H.Keys (J \/ 2) > H.Keys (J) loop/while J > 2 and then H.Keys (J \/ 2) > H.Keys (J) loop/' || fail=1
# Sift down picks the larger child.
mutate down-larger-child wait_deadlines.adb 's/H.Keys (2 \* J + 1) < H.Keys (2 \* J)/H.Keys (2 * J + 1) > H.Keys (2 * J)/' || fail=1
# A swap that forgets one place.
mutate swap-place wait_deadlines.adb 's/H.Place (H.Items (B)) := B;/null;/' || fail=1
# Remove that leaves the thread's place.
mutate remove-keeps-place wait_deadlines.adb 's/H.Place (T) := 0;/null;/' || fail=1
# Remove that always sifts down (a moved small key stays low).
mutate remove-no-up wait_deadlines.adb 's/if P > 1 and then H.Keys (P \/ 2) > H.Keys (P) then/if False then/' || fail=1
# Pop takes the root before its deadline.
mutate pop-early wait_deadlines.adb 's/if H.Keys (1) <= Now then/if H.Keys (1) <= Now or else H.Size = 1 then/' || fail=1
exit $fail
