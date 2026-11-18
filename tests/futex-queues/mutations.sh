#!/usr/bin/env bash
# Mutation check for the futex proofs: each mutant must FAIL to prove.
# Run from the Nix shell: tests/futex-queues/mutations.sh [kernel-src-dir]
set -u
here=$(cd "$(dirname "$0")" && pwd)
ksrc=${1:-$here/../../kernel/src}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
# alr needs the kernel's workspace.
cd "$here/../../kernel"

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
    if alr exec -- gnatprove -P"$dir/t.gpr" -u futex_queues_small.ads futex_protocol.adb futex_keys.adb \
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
mutate no-compare futex_protocol.adb 's/Slept := S.T (I).Seen = S.T (I).Expected and then S.T (I).Live;/Slept := S.T (I).Live;/' || fail=1
# FUTEX_WAKE without the bucket lock (may run between load and commit).
mutate unlocked-wake futex_protocol.ads 's/Pre  => Inv (S) and then not S.Holder,$/Pre  => Inv (S),/' || fail=1
# A store that forgets its wake obligation.
mutate store-no-pending futex_protocol.adb 's/S.Pending := True;/null;/' || fail=1
# Shared futexes (IPC-010).
# A wait that does not look whether the grant life ended.
mutate load-ignores-end futex_protocol.adb 's/Seen => S.Value, Live => not S.Ended);/Seen => S.Value, Live => True);/' || fail=1
# A sweep that leaves sleepers.
mutate sweep-leaves-sleepers futex_protocol.adb '/procedure Sweep (S : in out State) is/,/end Sweep;/ s/if S.T (I).P = Sleeping then/if False then/' || fail=1
# Ending a life wakes waiters of every life of the slot.
mutate revoke-any-life futex_queues.ads 's/(K.Space = Shared_Space and then K.Owner = Slot and then K.Generation = Generation)/(K.Space = Shared_Space and then K.Owner = Slot)/' || fail=1
# Ending a life wakes that life's waiters of every grant slot.
# (Dropping the space check alone is an equivalent mutant: a private key's
# life is always zero and a live grant's never is; Prove_Private_Kept
# states that.)
mutate revoke-any-slot futex_queues.ads 's/(K.Space = Shared_Space and then K.Owner = Slot and then K.Generation = Generation)/(K.Space = Shared_Space and then K.Generation = Generation)/' || fail=1
# A grant's shared keys spread over buckets (a one-bucket sweep would miss some).
mutate grant-spread futex_keys.adb 's/                return Grant_Bucket (K.Owner);/                return Bucket_Index ((Unsigned_64 (K.Owner) + K.Address) mod Bucket_Count);/' || fail=1
exit $fail
