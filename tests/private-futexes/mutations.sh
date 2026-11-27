#!/usr/bin/env bash
# Mutation check for the private futex proofs: each mutant must FAIL to prove.
# Run from the Nix shell: tests/private-futexes/mutations.sh [kernel-src-dir]
set -u
here=$(cd "$(dirname "$0")" && pwd)
ksrc=${1:-$here/../../kernel/src}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
cd "$here/../../kernel"   # alr needs the kernel's workspace

mutate() {  # name file sed-expression
    local name=$1 file=$2 expr=$3 dir="$work/$1"
    mkdir -p "$dir/src"
    cp "$ksrc"/private_futexes.ad? "$here"/word_protocol.ad? "$here"/main.adb "$dir/src/"
    sed -i "$expr" "$dir/src/$file"
    if cmp -s "$dir/src/$file" "$ksrc/$file" 2>/dev/null || cmp -s "$dir/src/$file" "$here/$file" 2>/dev/null; then
        echo "MUTANT $name: NOT APPLIED"; return 1
    fi
    sed "s|(\".\", Kernel_Src)|(\"$dir/src\")|; s|for Object_Dir use \"build\"|for Object_Dir use \"$dir/build\"|; s|for Exec_Dir use \"build\"|for Exec_Dir use \"$dir/build\"|" \
        "$here/private_futexes_tests.gpr" > "$dir/t.gpr"
    if alr exec -- gnatprove -P"$dir/t.gpr" -u private_futexes.adb word_protocol.adb \
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
# Wake the newest waiter instead of the oldest.
mutate newest-first private_futexes.adb 's/and then (not Found or else T.Ticket (U) < T.Ticket (S))/and then (not Found or else T.Ticket (U) > T.Ticket (S))/' || fail=1
# A lookup that ignores the word (wakes another word's waiter).
mutate ignore-word private_futexes.adb '/procedure Find_Oldest/,/end Find_Oldest;/ s/if T.Used (U) and then T.Word (U) = W$/if T.Used (U)/' || fail=1
# Reuse a ticket.
mutate ticket-reuse private_futexes.adb 's/T.Next_Ticket := T.Next_Ticket + 1;/null;/' || fail=1
# Remove without checking the slot still holds the thread.
mutate unchecked-remove private_futexes.adb 's/Removed := T.Used (S) and then T.Thread (S) = Thread;/Removed := T.Used (S);/' || fail=1
# A batch that takes one more than its bound.
mutate batch-overrun private_futexes.adb 's/exit when Count = Max;/null;/' || fail=1
# A wake-all batch that stops at the first waiter (a lost wake).
mutate batch-stops-early private_futexes.adb '/All_Of_Them then/,/else/ s/exit when Count = Max;/exit;/' || fail=1
# A batch that does not clear what it took.
mutate batch-keeps private_futexes.adb '/All_Of_Them then/,/else/ s/T.Used (U) := False;/null;/' || fail=1
# The kernel sleeps without comparing the loaded word.
mutate no-compare word_protocol.adb 's/Slept := S.T (I).Seen = S.T (I).Expected;/Slept := True;/' || fail=1
# A store that forgets its wake obligation.
mutate store-no-pending word_protocol.adb 's/S.Pending := True;/null;/' || fail=1
# Wake all that takes one batch only, of one waiter.
mutate wake-all-one-batch word_protocol.adb 's/Take_Batch (S.Tab, The_Word, Chunk, True, Taken, Count);/Take_Batch (S.Tab, The_Word, 1, True, Taken, Count); exit;/' || fail=1
exit $fail
