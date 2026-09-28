#!/usr/bin/env bash
# Mutation check for the futex proofs: each mutant must FAIL to prove.
# Run from the Nix shell: tests/futex-queues/mutations.sh [kernel-src-dir]
set -u
here=$(cd "$(dirname "$0")" && pwd)
ksrc=${1:-$here/../../kernel/src}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

mutate() {  # name file sed-expression
    local name=$1 file=$2 expr=$3 dir="$work/$1"
    mkdir -p "$dir/src"
    cp "$ksrc"/futex_queues.ad? "$ksrc"/futex_keys.ad? "$here"/futex_queues_small.ads "$here"/futex_queues_large.ads "$here"/futex_protocol.ad? "$here"/main.adb "$dir/src/"
    sed -i "$expr" "$dir/src/$file"
    if cmp -s "$dir/src/$file" "$ksrc/$file" 2>/dev/null || cmp -s "$dir/src/$file" "$here/$file" 2>/dev/null; then
        echo "MUTANT $name: NOT APPLIED"; return 1
    fi
    sed "s|\"../../kernel/src\"|\"$dir/src\"|; s|for Object_Dir use \"build\"|for Object_Dir use \"$dir/build\"|; s|for Exec_Dir use \"build\"|for Exec_Dir use \"$dir/build\"|; s|(\".\", Kernel_Src)|(\"$dir/src\")|" \
        "$here/futex_queues_tests.gpr" > "$dir/t.gpr"
    if alr exec -- gnatprove -P"$dir/t.gpr" -u futex_queues_small.ads futex_protocol.adb \
         --level=2 --timeout=30 -j0 --report=fail >"$dir/log" 2>&1 \
       && ! grep -q "medium\|high" "$dir/log"; then
        echo "MUTANT $name: SURVIVED (proof still passes)"; return 1
    fi
    if ! grep -q "medium\|high" "$dir/log"; then
        echo "MUTANT $name: ERROR (no proof failure reported)"; tail -5 "$dir/log"; return 1
    fi
    echo "MUTANT $name: killed -- $(grep -m1 'medium\|high' "$dir/log")"
}

fail=0
# Wake the newest waiter instead of the oldest.
mutate newest-first futex_queues.adb 's/B.S (I).Ticket < B.S (From_Slot).Ticket/B.S (I).Ticket > B.S (From_Slot).Ticket/' || fail=1
# Wake a waiter without checking its key.
mutate ignore-key futex_queues.adb 's/if B.S (I).Used and then B.S (I).K = K and then/if B.S (I).Used and then/' || fail=1
# Reuse a ticket (no increment).
mutate ticket-reuse futex_queues.adb 's/B.Next_Ticket := Ticket + 1;/null;/' || fail=1
# Remove a slot without checking it still holds the waiter.
mutate unchecked-remove futex_queues.adb 's/Removed := B.S (At_Slot).Used and then B.S (At_Slot).Waiter = W;/Removed := B.S (At_Slot).Used;/' || fail=1
# Kernel sleeps without comparing the loaded word.
mutate no-compare futex_protocol.adb 's/Slept := S.T (I).Seen = S.T (I).Expected;/Slept := True;/' || fail=1
# FUTEX_WAKE without the bucket lock (may run between load and commit).
mutate unlocked-wake futex_protocol.ads 's/Pre  => Inv (S) and then not S.Holder,$/Pre  => Inv (S),/' || fail=1
# A store that forgets its wake obligation.
mutate store-no-pending futex_protocol.adb 's/S.Pending := True;/null;/' || fail=1
exit $fail
